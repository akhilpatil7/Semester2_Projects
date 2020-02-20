import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.lang.reflect.AnnotatedArrayType;
import java.security.AlgorithmConstraints;
import java.sql.Array;

public class Homework2_4 {
	private static int[] auxBU;
	private static int[] auxUB;
	
	public static int sortUB(int[] nums) {
		int res=0;
		auxUB=new int[nums.length];
		res=sortUB(nums,0,nums.length-1);
		return res;
		}
	
	
	// from bottom to top
	public static int sortBU(int[] nums) {
		int res=0;
		auxBU=new int[nums.length];
		for(int size=1;size<nums.length;size*=2)
			for(int left=0;left<nums.length-size;left+=size+size)
				res+=mergeBU(nums, left, left+size-1, Math.min(left+2*size-1, nums.length-1));
		return res;
	}
	
	
	//from top to bottom
	public static int sortUB(int[] nums,int left,int right) {
		int res=0;
		if(right>left) {
			int mid=left+(right-left)/2;
			// sort the left part
			res+=sortUB(nums,left,mid);
			// sort the right part
			res+=sortUB(nums,mid+1,right);	
			// merge the 2 subarray
			res+=mergeUB(nums, left, mid, right);
		}
		return res;
	}
	
	// merge BU
	public static int mergeBU(int[] nums,int left,int mid,int right) {
		int res=0;
		int i=left,j=mid+1;
		// copy the value to aux array
		for(int k=left;k<=right;k++) {
			auxBU[k]=nums[k];
		}
		
		// merge
		for(int k=left;k<=right;k++) {
			if(i>mid)
				nums[k]=auxBU[j++];
			else if(j>right)
				nums[k]=auxBU[i++];
			else if(auxBU[i]<auxBU[j]) {
				nums[k]=auxBU[i++];
				res++;
			}
			else {
				nums[k]=auxBU[j++];
				res++;
			}
		}
		return res;
	}
	
	// merge UB
	public static int mergeUB(int[] nums,int left,int mid,int right) {
		int res=0;
		int i=left,j=mid+1;
		// copy the value to aux array
		for(int k=left;k<=right;k++) {
			auxUB[k]=nums[k];
		}
		
		// merge
		for(int k=left;k<=right;k++) {
			if(i>mid)
				nums[k]=auxUB[j++];
			else if(j>right)
				nums[k]=auxUB[i++];
			else if(auxUB[i]<auxUB[j]) {
				nums[k]=auxUB[i++];
				res++;
			}
			else {
				nums[k]=auxUB[j++];
				res++;
			}
		}
		return res;
	}
	
	
	public static void main(String[] args) {
		try {
			int[] result=new int[12];
			int[] compNum=new int[12];
			int[] compNum1=new int[12];
			int[] array=null;
			String[] fileName= {"data0.1024","data0.2048","data0.4096","data0.8192","data0.16384","data0.32768",
					"data1.1024","data1.2048","data1.4096","data1.8192","data1.16384","data1.32768"};
			
			for(int i=0;i<5;i++) {

				for(int k=0;k<fileName.length;k++) {
					// read the data from the file
					File file=new File("data/" + fileName[k]);
					FileReader fileReader = new FileReader(file);
					
					int size=Integer.valueOf(fileName[k].substring(6,fileName[k].length()));
					array=new int[size];
					BufferedReader bufferedReader = new BufferedReader(fileReader);
					String line;
					int index=0;
					// to store the data in the file into a array
					while ((line = bufferedReader.readLine()) != null) {
						array[index]=Integer.valueOf(line);
						index++;
					}
					
					fileReader.close();
	
					long startTime=System.nanoTime();
					int num=sortUB(array);
					int num1=sortBU(array);
					long endTime=System.nanoTime();
					
					result[k]+=(endTime-startTime);
					compNum[k]+=num;
					compNum1[k]+= num1;
					
					
				}
			}
			

			for(int i=0;i<12;i++) {
				//print the number of comparisons 
				System.out.println(String.valueOf(compNum1[i]) +" compares in the dataset using BU Non-recursive merge sort");
				System.out.println(String.valueOf(compNum[i]) +" compares in the dataset using UB Recursive merge sort");

				System.out.println("Running time is: "+String.valueOf(result[i]/5000)+" μs");

				System.out.println();
			}

		}catch(IOException e) {
			e.printStackTrace();
		}
	}

}

//Source
//https://algs4.cs.princeton.edu/14analysis/Mergesort.java.html
//https://algs4.cs.princeton.edu/22mergesort/Merge.java.html
//References : https://link.springer.com/content/pdf/10.1007%2FBF01294131.pdf