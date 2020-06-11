module simplified_sha256(
 input logic  clk, reset_n, start,
 input logic [31:0] message[16], 
 input logic [31:0] in[8],
 input logic [31:0] k[0:63],
 input logic decision,
 output logic done,
 output logic [31:0] sha256[8]);

enum logic [2:0] {IDLE, BLOCK, PIPE1, PIPE2, COMPUTE, WRITE, DONE} state;
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;
logic [31:0] a, b, c, d, e, f, g, hash, P;
logic [ 6:0] num;
logic [31:0] w[16];

function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, P);
    logic [31:0] S1, S0, ch, maj, t1, t2; 
	begin
		S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
		ch = (e & f) ^ ((~e) & g);
		t1 = S1 + ch + P;
		S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
		maj = (a & b) ^ (a & c) ^ (b & c);
		t2 = S0 + maj;
		sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
	end
endfunction

function logic [31:0] wi;
	logic [31:0] S1, S0;
	begin
		S0 = rightrotate(w[1], 7) ^ rightrotate(w[1], 18) ^ (w[1] >> 3);
		S1 = rightrotate(w[14], 17) ^ rightrotate(w[14], 19) ^ (w[14] >> 10);
		wi = w[0] + S0 + w[9] + S1;
	end
endfunction	

function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [ 7:0] r);
begin
	rightrotate = (x >> r) | (x << (32-r));
end
endfunction

always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    state <= IDLE;
  end 
  else case (state)
    IDLE: begin 
       if(start) begin
			if(decision) begin
				h0 <= in[0];
				h1 <= in[1];
				h2 <= in[2];
				h3 <= in[3];
				h4 <= in[4];
				h5 <= in[5];
				h6 <= in[6];
				h7 <= in[7]; 	
			
				a <= in[0];
				b <= in[1];
				c <= in[2];
				d <= in[3];
				e <= in[4];
				f <= in[5];
				g <= in[6];
				hash <= in[7];
			end
			else begin
				h0 <= 32'h6a09e667;
				h1 <= 32'hbb67ae85;
				h2 <= 32'h3c6ef372;
				h3 <= 32'ha54ff53a;
				h4 <= 32'h510e527f;
				h5 <= 32'h9b05688c;
				h6 <= 32'h1f83d9ab;
				h7 <= 32'h5be0cd19;

				a <= 32'h6a09e667;
				b <= 32'hbb67ae85;
				c <= 32'h3c6ef372;
				d <= 32'ha54ff53a;
				e <= 32'h510e527f;
				f <= 32'h9b05688c;
				g <= 32'h1f83d9ab;
				hash <= 32'h5be0cd19;
			end			
			num <= 7'b0;
			state <= BLOCK;
       end
    end
		
    BLOCK: begin
		for(int n=0; n<16; n++) w[n] <= message[n];
		state <= PIPE1;
    end

	 PIPE1: begin
			P <= w[0] + k[0] + hash;
			for(int n=0; n<15; n++) w[n] <= w[n + 1];
			w[15] <= wi;
			state <= PIPE2;
	 end

	 PIPE2: begin
		{a, b, c, d, e, f, g, hash} <= sha256_op(a, b, c, d, e, f, g, P);
		P <= w[0] + k[1] + g;
		for(int n=0; n<15; n++) w[n] <= w[n + 1];
		w[15] <= wi;
		num <= num+1;
		state <= COMPUTE;
	 end
	 
	 
    COMPUTE: begin
		if(num < 64) begin
			P <= w[0] + k[num+1] + g;
			{a, b, c, d, e, f, g, hash} <= sha256_op(a,b,c,d,e,f,g,P);
			for(int j=0;j<15;j++) w[j] <= w[j+1];
			w[15] <= wi;
			num <= num + 1;
			state <= COMPUTE;
		end
	   else begin
		 h0 <= a + h0;
		 h1 <= b + h1;
		 h2 <= c + h2;
		 h3 <= d + h3;
		 h4 <= e + h4;
		 h5 <= f + h5;
		 h6 <= g + h6;
		 h7 <= hash + h7;
		 num <= 0;
		 state <= WRITE;
     end
	 end
	 
    WRITE: begin
		sha256[0] <= h0;
		sha256[1] <= h1;
		sha256[2] <= h2;
		sha256[3] <= h3;
		sha256[4] <= h4;
		sha256[5] <= h5;
		sha256[6] <= h6;
		sha256[7] <= h7;
		state <= DONE;
		end
    
	 DONE: begin
		state <= IDLE;
	end
   endcase
  end

assign done = (state == DONE);

endmodule
