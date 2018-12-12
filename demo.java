// ESOP10 Dillig Dillig Aiken, adapted to simple java syntax.
// Launch the front-end : ./java2horn demo.java -o demo.smt2

class init_cte{
    
    static int i,size,tmp,c,u;
    static int a[] ;
    
    static void main() {
	size=Support.random();
	Support.assume(0 < size);
	
	i=0;
	c=Support.random();
	
	while(i< size){
	    a[i]=c;
	    i=i+1;
	}

	i=0; //Property to prove !
	while(i < size){
	    u=a[i];
	    assert(u==c);
	    i=i+1;
	}
    }
}
