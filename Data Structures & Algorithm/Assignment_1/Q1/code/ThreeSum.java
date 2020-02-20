
import java.util.Arrays;
import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;



public class ThreeSum {
	// Naive implementation of 3sum with Brute Force
	public static int naiveCount(int[] a) {
		int N=a.length;
		int cnt=0;
		for(int i=0;i<N;i++)
			for(int j=i+1;j<N;j++)
				for(int k=j+1;k<N;k++)
					if(a[i]+a[j]+a[k]==0)
						cnt++;
		return cnt;
	}
	

	// Sophisticated implementation of 3 sum 
	public static int sophisticatedalgCount(int[] a) {
		Arrays.sort(a);
		int N=a.length;
		int cnt=0;
		for(int i=0;i<N;i++)
			for(int j=i+1;j<N;j++)
				if(Arrays.binarySearch(a, -a[i]-a[j])>j)
					cnt++;
		return cnt;
		
	}
	
	public static void main(String[] args) {
		try {
			// Getting the files for testing the run time.
			int[] fileName= {8,32,128,512,1024,4096,4192,8192};
			for(int k=0;k<8;k++) {  
				// get contents from file
				
				//File resourceFile = new File("../myFile.txt");
				
				File file=new File("data/" + String.valueOf(fileName[k])+"int.txt");
				FileReader fileReader = new FileReader(file);
				int[] array=new int[fileName[k]];
				BufferedReader bufferedReader = new BufferedReader(fileReader);
				String line;
				int i=0;
				// parsing data by line in integer and storing it in an array.
				while ((line = bufferedReader.readLine()) != null) {
					array[i]=Integer.valueOf(line);
					i++;
				}
				fileReader.close();
				// checking run time for Naive.
				
				long startTime=System.currentTimeMillis();
				int cnt=ThreeSum.naiveCount(array);
				long endTime=System.currentTimeMillis();
				// output the result and the running time.
				System.out.println("ThreeSum time naiveCount cost is: "+(endTime-startTime)+"ms");
						
				// checking run time for Sophisticated
				long startTimeSophisticated=System.currentTimeMillis();
				int cnt1=ThreeSum.sophisticatedalgCount(array);
				long endTimeSophisticated=System.currentTimeMillis();
				// output the result and the running time.
				System.out.println("ThreeSum Sophisticatedalg time cost is: "+(endTimeSophisticated-startTimeSophisticated)+"ms");
				
			}
			
		}
		catch(IOException e) {
			e.printStackTrace();
		}
		

		
	}
}


// Source of code:
//https://www.geeksforgeeks.org/find-a-triplet-that-sum-to-a-given-value/
