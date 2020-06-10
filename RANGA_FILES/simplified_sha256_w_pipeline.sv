module simplified_sha256 #(parameter integer NUM_OF_WORDS = 20)(
 input logic  clk, reset_n, start,
 input logic  [15:0] message_addr, output_addr,
 output logic done, mem_clk, mem_we,
 output logic [15:0] mem_addr,
 output logic [31:0] mem_write_data,
 input logic [31:0] mem_read_data);

// FSM state variables 
enum logic [2:0] {IDLE, READ, BLOCK, COMPUTE_P1, COMPUTE_P2, WRITE} state;

// NOTE : Below mentioned frame work is for reference purpose.
// Local variables might not be complete and you might have to add more variables
// or modify these variables. Code below is more as a reference.

// Local variables
logic [31:0] w[16]; //Changed from 64
logic [31:0] s0, s1;
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;
logic [31:0] a, b, c, d, e, f, g, h;
logic [ 7:0] i, j, n, nn;
logic [15:0] offset; // in word address
logic [ 7:0] num_blocks;
logic        cur_we;
logic [15:0] cur_addr;
logic [31:0] cur_write_data;
logic [31:0] P;

// SHA256 K constants
parameter int k[0:63] = '{
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};

parameter integer SIZE = NUM_OF_WORDS * 32;
int num_words = NUM_OF_WORDS;
assign num_blocks = determine_num_blocks(SIZE); 

// Note : Function defined are for reference purpose. Feel free to add more functions or modify below.
// Function to determine number of blocks in memory to fetch
// Input to the function is size of message in bits ( in our case 640)
function logic [31:0] determine_num_blocks(input logic [31:0] size_of_msg);

  // Student to add function implementation
  int num_zeroes;

  for (num_zeroes = 0; num_zeroes < 448; num_zeroes++) begin
	  if ((size_of_msg + 1 + num_zeroes + 64) % 512 == 0) begin
		  determine_num_blocks = ((size_of_msg + 1 + num_zeroes + 64)/512);
		  break;
	  end
  end
endfunction

// SHA256 hash round
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, P);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
    // Student to add remaning code below
    ch = (e & f) ^ ((~e) & g); 
    t1 = S1 + ch + P; 
    S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;
    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
end
endfunction

function logic [31:0] wtnew;
	logic [31:0] var_s0, var_s1;
	var_s0 = rightrotate(w[1],7) ^ rightrotate(w[1], 18) ^ (w[1] >> 3);
	var_s1 = rightrotate(w[14], 17) ^ rightrotate(w[14], 19) ^ (w[14] >> 10);
	wtnew = w[0] + var_s0 + w[9] + var_s1;
endfunction

// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
assign mem_clk = clk;
assign mem_addr = cur_addr + offset;
assign mem_we = cur_we;
assign mem_write_data = cur_write_data;


// Right Rotation Example : right rotate input x by r
// Lets say input x = 1111 ffff 2222 3333 4444 6666 7777 8888
// lets say r = 4
// x >> r  will result in : 0000 1111 ffff 2222 3333 4444 6666 7777 
// x << (32-r) will result in : 8888 0000 0000 0000 0000 0000 0000 0000
// final right rotate expression is = (x >> r) | (x << (32-r));
// (0000 1111 ffff 2222 3333 4444 6666 7777) | (8888 0000 0000 0000 0000 0000 0000 0000)
// final value after right rotate = 8888 1111 ffff 2222 3333 4444 6666 7777
// Right rotation function
function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [ 7:0] r);
		  begin
			  rightrotate = (x >> r) | (x << (32 - r));
		  end
   // Student to add function implementation
endfunction


// SHA-256 FSM 
// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function
// and write back hash value back to memory
always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    cur_we <= 1'b0;
    state <= IDLE;
    cur_addr <= message_addr; //change1
    offset <= 0;
  end 
  else case (state)
    // Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
    IDLE: begin 
       if(start) begin
       // Student to add rest of code
    		h0 <= 32'h6a09e667;
    		h1 <= 32'hbb67ae85;
    		h2 <= 32'h3c6ef372;
    		h3 <= 32'ha54ff53a;
    		h4 <= 32'h510e527f;
    		h5 <= 32'h9b05688c;
    		h6 <= 32'h1f83d9ab;
    		h7 <= 32'h5be0cd19;


    		cur_we <= 0;
		cur_addr <= message_addr;
		offset <= 0; //change2
		state <= BLOCK;
		j <= 0;
		i <= 0;
		n <= 0;
		nn <= 0;
	end else begin
		state <= IDLE;
	end
    end

    // SHA-256 FSM 
    // Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function    
    // and write back hash value back to memory
    BLOCK: begin
	    a <= h0;
	    b <= h1;
	    c <= h2;
	    d <= h3;
	    e <= h4;
	    f <= h5;
	    g <= h6;
	    h <= h7;
	// Fetch message in 512-bit block size
	// For each of 512-bit block initiate hash value computation
	    if (j < num_blocks) begin
		    // Two cases of reading - we either load full 16 words or
		    // load a few words and then do zero padding
		    if (num_words/16 != 0) begin
			    if(n < 18) begin
				    if ( n == 0) begin
					    offset <= j*16;
					    n <= n + 1;
					    state <= BLOCK;
				    end else begin
					    offset <= j*16 + n;
					    if ( n != 1) begin
						    w[n-2] <= mem_read_data;
					    end
					    n <= n + 1;
					    state <= BLOCK;
				    end
			    end else begin
				    j <= j + 1;
				    num_words <= num_words - 16;
				    nn <= num_words - 15;
				    n <= 0;
				    P <= w[0] + k[0] + h;
				    for (int v = 0; v < 15; v++) w[v] <= w[v+1];
				    w[15] <= wtnew();
				    state <= COMPUTE_P1;
			    end
		    end else begin
			    // we have less than 16 words left in message
			    // Load remaining words
			    if(n < num_words + 2) begin
				    if ( n == 0) begin
					    offset <= j*16;
					    n <= n + 1;
					    state <= BLOCK;
				    end else begin
					    offset <= j*16 + n;
					    if ( n != 1) begin
						    w[n-2] <= mem_read_data;
					    end
					    n <= n + 1;
					    state <= BLOCK;
				    end
				//Finished loading all actual message bits
				// Start 1 + zero padding + size storage
			    end else begin
				    // Load 1 + zero-padding + size
				    w[num_words] <= 32'h80000000;
				    if (nn < 15) begin
					    w[nn] <= 32'h00000000;
					    w[15] <= SIZE;
					    nn <= nn + 1;
					    state <= BLOCK;
				    end else begin
					    P <= w[0] + k[0] + h;
					    for (int v = 0; v < 15; v++) w[v] <= w[v+1];
					    w[15] <= wtnew();
					    state <= COMPUTE_P1;
					    j <= j + 1;
				    end
			    end
		    end
	    end else begin
		    state <= WRITE;
		    i <= 0;
	    end
    end


    // For each block compute hash function
    // Go back to BLOCK stage after each block hash computation is completed and if
    // there are still number of message blocks available in memory otherwise
    // move to WRITE stage
    COMPUTE_P1: begin
	    {a, b, c, d, e, f, g, h} <= sha256_op(a,b,c,d,e,f,g,P);
	    P <= w[0] + k[1] + g;
	    for(int v = 0; v < 15; v++) begin
		    w[v] <= w[v+1];
	    end
	    w[15] <= wtnew();
	    i <= i + 1;
	    state <= COMPUTE_P2;
    end
    COMPUTE_P2: begin
	// 64 processing rounds steps for 512-bit block
	if (i < 64) begin
	       {a,b,c,d,e,f,g,h} <= sha256_op(a,b,c,d,e,f,g,P);
	       P <= w[0] + k[i+1] + g;
	       i <= i + 1;
	       for (int w_iter = 0; w_iter < 15; w_iter++) begin
		       w[w_iter] <= w[w_iter+1];
	       end
	       w[15] <= wtnew();
	       state <= COMPUTE_P2;
       end else begin
	       i <= 0;
	       h0 <= h0 + a;
	       h1 <= h1 + b;
	       h2 <= h2 + c;
	       h3 <= h3 + d;
	       h4 <= h4 + e;
	       h5 <= h5 + f;
	       h6 <= h6 + g;
	       h7 <= h7 + h;
	       state <= BLOCK;
       end
    end
    // h0 to h7 each are 32 bit hashes, which makes up total 256 bit value
    // h0 to h7 after compute stage has final computed hash value
    // write back these h0 to h7 to memory starting from output_addr
    WRITE: begin
	    if (i < 8) begin
		    cur_we <= 1;
		    cur_addr <= output_addr;
		    offset <= i;
		    case(i)
			    0: cur_write_data <= h0;
			    1: cur_write_data <= h1;
			    2: cur_write_data <= h2;
			    3: cur_write_data <= h3;
			    4: cur_write_data <= h4;
			    5: cur_write_data <= h5;
			    6: cur_write_data <= h6;
			    7: cur_write_data <= h7;
			    default: cur_write_data <= h0;
		    endcase
		    i <= i + 1;
		    state <= WRITE;
	    end
	    else begin
		    state <= IDLE;
	    end
    end
   endcase
  end

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (state == IDLE);

endmodule

