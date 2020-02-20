import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.io.*;

public class Faster3sum {
	public static int hashFaster3Sum(int[] numbs) {
		int res=0;
		// declare the hashmap
		Map<Integer,Integer> map=new HashMap<Integer,Integer>();
		for(int i=0;i<numbs.length;i++) {
			map.clear();
			// It will go to the worst case and perform as a quadratic algorithm
			if(i==0||numbs[i-1]<numbs[i]) {
				for(int j=i+1;j<numbs.length;j++) {
					if(map.containsKey(numbs[j]+numbs[i])) {
						res++;
						// to skip the duplicate
						while(j<numbs.length-1&&numbs[i]==numbs[i+1])
							j++;
					}else {
						map.put(-numbs[i]-numbs[j],numbs[i]+numbs[j] );
					}
				}
			}
			
		}
		return res;
	}
	
	public static void main(String[] args) {
		try {
			// write a loop to read each test data.
			int[] fileName= {8,32,128,512,1024,4096,4192,8192};
			for(int j=0;j<8;j++) 
			{
				File file=new File("data/" + String.valueOf(fileName[j])+"int.txt");
				FileReader fileReader = new FileReader(file);
				int[] array=new int[fileName[j]];
				BufferedReader bufferedReader = new BufferedReader(fileReader);
				String line;
				int i=0;
				// read the data from the file
				while ((line = bufferedReader.readLine()) != null) {
					array[i]=Integer.valueOf(line);
					i++;
				}
				//int[] array1= {-1,-1,0,0,1,1,1};
				Arrays.sort(array);
				//fileReader.close();
				long startT=System.currentTimeMillis();
				// do the 3 sum algorithm.
				int result=Faster3sum.hashFaster3Sum(array);
				long endT=System.currentTimeMillis();
				// output the running time and the result.
				System.out.println("Time cost is "+String.valueOf((endT-startT)) + " ms");
			}
			
		}
		catch(IOException e) {
			e.printStackTrace();
		}
		
	}
}

