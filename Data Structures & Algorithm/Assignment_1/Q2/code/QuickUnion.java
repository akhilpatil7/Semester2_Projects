import java.io.*;

public class QuickUnion {
    private int[] parent;  // parent[i] = parent of i
    private int count;     // number of components


    // initialize the array.
    public QuickUnion(int n) {
        parent = new int[n];
        count = n;
        for (int i = 0; i < n; i++) {
            parent[i] = i;
        }
    }

    // return the count of the components
    public int count() {
        return count;
    }
  
    // return the value of the node p.
    public int find(int p) {
        //validate(p);
        while (p != parent[p])
            p = parent[p];
        return p;
    }

    // return if two nodes has been connected
    public boolean connected(int p, int q) {
        return find(p) == find(q);
    }

    // union the two nodes
    public void union(int p, int q) {
        int rootP = find(p);
        int rootQ = find(q);
        if (rootP == rootQ) return;
        parent[rootP] = rootQ; 
        count--;
    }

    
    public static void main(String[] args) {
    	try {
    			int[] temp1= {8,32,128,512,1024,4096,8192};
			for(int k=0;k<7;k++) {
				int N=8192;
				QuickUnion quickUnion=new QuickUnion(N);
				File file=new File("data/" +String.valueOf(temp1[k])+"pair.txt");
				FileReader fileReader=new FileReader(file);
				BufferedReader bufferedReader = new BufferedReader(fileReader);
				String line;
				
				int num=temp1[k];
				int[] array1=new int[num];
				int[] array2=new int[num];
				int i=0;
				while ((line = bufferedReader.readLine()) != null) {
					
					
					String[] temp=line.split("\\s+",2);
					array1[i]=Integer.valueOf(temp[0]);
					array2[i]=Integer.valueOf(temp[1]);
					i++;
				
				}
				
				long startT=System.nanoTime();
				for(int j=0;j<num;j++) {
					if(quickUnion.connected(array1[j], array2[j]))
						continue;
					quickUnion.union(array1[j], array2[j]);
				}

				
				long endT=System.nanoTime();
				
				System.out.print(quickUnion.count()+" Components");
				System.out.print("QuickUnion time cost is: "+(endT-startT)/100+" μs"+"\n");
				bufferedReader.close();
			}
			
		}
		catch(IOException e) {
			e.printStackTrace();
		}
    }


}

// Source of code:
//https://algs4.cs.princeton.edu/15uf/QuickUnionUF.java.html
