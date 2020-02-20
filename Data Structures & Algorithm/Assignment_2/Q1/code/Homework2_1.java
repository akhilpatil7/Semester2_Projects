import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class Homework2_1 {

	public static int shellsort(int[] nums) {
		int[] hSequence= {7,3,1}; // sequence for h sort
		int comNum=0;
		for(int i=0;i<3;i++) { // loop for getting the H value in place
			int h=hSequence[i];
			for(int j=h;j<nums.length;j++) { 
				for(int k=j;k>=h;k-=h) { // sorting at h positions 
					comNum++; // recording the number of comparisons
					if(nums[k]<nums[k-h]) // comparing the two numbers at k th & k-h th positions
						swap(nums,k,k-h);
					else
						break;
				}
			}
		}
		return comNum;
	}
	
	public static int insertSort(int[] nums) {
		int comNum=0;
		for(int i=0;i<nums.length;i++) { // looping till array length -1
			for(int j=i;j>0;j--) {
				comNum++; // recording the number of comparisons
				if(nums[j]<nums[j-1]) // comparing the two numbers at k th & k-h th positions
					swap(nums,j,j-1);
				else
					break;
			}
		}
		return comNum;
	}
	
	public static void swap(int[] nums,int p,int q) {
		int temp=nums[p];
		nums[p]=nums[q];
		nums[q]=temp;
	}
	
	public static void main(String[] args) {
		try {
			String[] fileName= {"data0.1024","data0.2048","data0.4096","data0.8192","data0.16384","data0.32768",
					"data1.1024","data1.2048","data1.4096","data1.8192","data1.16384","data1.32768"};
			for(int k=0;k<fileName.length;k++) {
				// read the data from the file
				File file=new File("data/" + fileName[k]);
				FileReader fileReader = new FileReader(file);
				
				int size=Integer.valueOf(fileName[k].substring(6,fileName[k].length()));
				int[] array=new int[size];
				BufferedReader bufferedReader = new BufferedReader(fileReader);
				String line;
				int index=0;
				// to store the data in the file into a array
				while ((line = bufferedReader.readLine()) != null) {
					array[index]=Integer.valueOf(line);
					index++;
				}
				
				fileReader.close();
				int[] tempArray=new int[array.length];
				System.arraycopy(array, 0, tempArray, 0, array.length);
				int shellRes=shellsort(array);
				int insertRes=insertSort(tempArray);
				
				System.out.println(shellRes+" comparisons made in shell sort in "+fileName[k]);
				System.out.println(insertRes+" comparisons made in insert sort in "+fileName[k]);
				System.out.println();
				
			}
		}catch(IOException e) {
			e.printStackTrace();
		}
		
	}
}



// Source
//https://algs4.cs.princeton.edu/21elementary/Shell.java.html
//https://algs4.cs.princeton.edu/21elementary/Insertion.java.html