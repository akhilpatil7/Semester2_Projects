import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;


public class Homework2_5 {

	// the value for CUTTOFF to insertion sort
	public static final int CUTOFF_VALUE=7;
	
	public static int quickSort(int[] nums) {
		int comNum=0;
		// shuffle the array.
		//shuffleArray(nums);
		// implement the quick sort
		comNum= quickSort(nums,0,nums.length-1);
		return comNum;
	}
	
	public static int quickSort(int[] nums,int lo, int hi) {
		// add a CUTOFF constrain
		int comNum=0;
		if(lo+CUTOFF_VALUE>=hi) {
			comNum=insertionSort(nums,lo,hi);
			return comNum;
		} else {
			// constraint
			if(hi<=lo) return 0;
		    // median-of-three 
			int mid=(lo+hi)/2;
			if(nums[lo]>nums[mid]) {
				swap(nums, lo, mid);
				comNum++;
			}
			if(nums[lo]>nums[hi]) {
				swap(nums, lo, hi);
				comNum++;
			}
			if(nums[mid]>nums[hi]) {
				swap(nums, mid, hi);
				comNum++;
			}
			
			swap(nums, mid, hi-1);
			// implement the partition
			int pivot=nums[hi-1];
			
			int i=lo,j=hi-1;
			
			while(true) {
				// scan from left
				while(i<nums.length&&nums[++i]<pivot)
					comNum++;
				while(j>0&&nums[--j]>pivot)
					comNum++;
				if(i<j)
					swap(nums, i, j);
				else 
					break;
			} 
			swap(nums, i, hi-1);
			
			// sort the left part
			comNum+=quickSort(nums,lo,i-1);
			// sort the right part
			comNum+=quickSort(nums,i+1,hi);
			return comNum;
		}
	}
	
	// swap the element
	public static void swap(int[] nums, int i,int j) {
		int temp=nums[i];
		nums[i]=nums[j];
		nums[j]=temp;
	}
	
	public static int insertionSort(int[] nums,int lo,int hi) {
		int comNum=0;
		for(int i=lo;i<=hi;i++) {
			for(int j=i;j>0;j--) {
				comNum++;
				if(nums[j]<nums[j-1])
					swap(nums,j,j-1);
				else
					break;
			}
		}
		return comNum;
	}
	
	
	// shuffle the array
	public static void shuffleArray(int[] nums) {
		Random random=ThreadLocalRandom.current();
		
		for(int i=nums.length-1;i>0;i--) {
			int index=random.nextInt(i+1);
			int a=nums[index];
			nums[index]=nums[i];
			nums[i]=a;
			
		}
	}
	

	public static void main(String[] args) {
		try {
			int[] result=new int[12];
			int[] compNum=new int[12];
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
					int num=quickSort(array);
					long endTime=System.nanoTime();
					
					result[k]+=(endTime-startTime);
					compNum[k]+=num;
				}
			}

			for(int i=0;i<12;i++) {
				System.out.println(String.valueOf(compNum[i]/5) +" compares in the dataset: ");
				System.out.println("Running time is: "+String.valueOf(result[i]/5000)+" μs");
				System.out.println();
			}
			
		}catch(IOException e) {
			e.printStackTrace();
		}
		
	}

}
//Source
//https://algs4.cs.princeton.edu/23quicksort/Quick.java.html
