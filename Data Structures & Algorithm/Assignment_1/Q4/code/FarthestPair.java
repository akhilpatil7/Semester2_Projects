import java.util.Random;

public class FarthestPair
{
    
    public static void main(String[] args)
    {
    	// create object of random class
        Random rand = new Random(21);
        int i;
        int N = 100;
        double[] array = new double[N]; 
        
        for (i = 0; i < N; i++)
            array[i] = rand.nextInt(100);        
		double min= array[0];
		double max= array[0];
        // note the start time 
		long startTime=System.nanoTime();
		// loop through the array and find the minimum and the maximum
		for(i=0;i<N;i++) {
			if(array[i]<min)
				min=array[i];
			if(array[i]>max)
				max=array[i];
		}
		long endTime=System.nanoTime();
        System.out.println();
        System.out.println("Farthest pair are numbers:" + min + " " + max );
        System.out.println("Time cost is "+String.valueOf((endTime-startTime)/100));
    }
}