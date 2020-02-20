import java.io.*;


public class WeightedQuickUnion {
	private int[] parent;   // parent[i] = parent of i
    private int[] size;     // size[i] = number of sites in subtree rooted at i
    private int count;      // number of components


    // initialize the array.
    public WeightedQuickUnion(int n) {
        count = n;
        parent = new int[n];
        size = new int[n];
        for (int i = 0; i < n; i++) {
            parent[i] = i;
            size[i] = 1;
        }
    }


    // return the count of the components
    public int count() {
        return count;
    }
  

    // return the value of the node p.
    public int findop(int p) {
        //validate(p);
        while (p != parent[p])
            p = parent[p];
        return p;
    }


    // return if two nodes has been connected
    public boolean connectedop(int p, int q) {
        return findop(p) == findop(q);
    }


    // union the two nodes
    public void union(int p, int q) {
        int rootP = findop(p);
        int rootQ = findop(q);
        if (rootP == rootQ) return;

        // make smaller root point to larger one
        if (size[rootP] < size[rootQ]) {
            parent[rootP] = rootQ;
            size[rootQ] += size[rootP];
        }
        else {
            parent[rootQ] = rootP;
            size[rootP] += size[rootQ];
        }
        count--;
    }


    
    public static void main(String[] args) {
    	try {
			// write a loop to read each test data.
    			int[] temp1= {8,32,128,512,1024,4096,8192};
			for(int k=0;k<7;k++) {
				int N=8192;
				WeightedQuickUnion weightedQuickUnion=new WeightedQuickUnion(N);
				// read the data from the file
				File file=new File("data/" +String.valueOf(temp1[k])+"pair.txt");
				FileReader fileReader=new FileReader(file);
				BufferedReader bufferedReader = new BufferedReader(fileReader);
				String line;
				
				
				int num=temp1[k];
				int[] arr1=new int[num];
				int[] arr2=new int[num];
				int i=0;
				// to store the data in the file into a array
				while ((line = bufferedReader.readLine()) != null) {
					
					String[] temp=line.split("\\s+",2);
					arr1[i]=Integer.valueOf(temp[0]);
					arr2[i]=Integer.valueOf(temp[1]);
					i++;
				
				}
				
				// do the Union Find and record the running time
				long startTime=System.nanoTime();
				for(int j=0;j<num;j++) {
					if(weightedQuickUnion.connectedop(arr1[j], arr2[j]))
						continue;
					weightedQuickUnion.union(arr1[j], arr2[j]);
					//System.out.println(arr1[j]+" "+arr2[j]);
				}
				
				long endTime=System.nanoTime();
				
				// output the result and the running time
				System.out.println(weightedQuickUnion.count()+" Components");
				System.out.print("WeightedQuickUnion time cost is: "+(endTime-startTime)/100+" μs"+"\n");
				bufferedReader.close();
			}
			
		}
		catch(IOException e) {
			e.printStackTrace();
		}

    }
}

// Source of code
//https://algs4.cs.princeton.edu/15uf/WeightedQuickUnionUF.java.html