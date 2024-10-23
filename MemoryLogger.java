import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class MemoryLogger {

    private static final Logger logger = LogManager.getLogger(MemoryLogger.class);

    public static void logMemoryUsage() {
        // Get the runtime instance
        Runtime runtime = Runtime.getRuntime();

        // Calculate memory stats
        long totalMemory = runtime.totalMemory();  // Total memory currently available to JVM
        long freeMemory = runtime.freeMemory();    // Free memory in the current allocation
        long maxMemory = runtime.maxMemory();      // Maximum memory JVM can allocate
        long usedMemory = totalMemory - freeMemory; // Memory currently used by the program

        // Convert to MB for better readability
        long totalMemoryMB = totalMemory / (1024 * 1024);
        long freeMemoryMB = freeMemory / (1024 * 1024);
        long maxMemoryMB = maxMemory / (1024 * 1024);
        long usedMemoryMB = usedMemory / (1024 * 1024);

        // Log the memory details
        logger.info("Memory Usage:");
        logger.info("Total Memory: {} MB", totalMemoryMB);
        logger.info("Free Memory: {} MB", freeMemoryMB);
        logger.info("Used Memory: {} MB", usedMemoryMB);
        logger.info("Max Memory: {} MB", maxMemoryMB);
    }

    public static void main(String[] args) {
        // Simulate memory usage logging during program execution
        for (int i = 0; i < 10; i++) {
            logMemoryUsage();

            // Simulate some work and sleep for a bit
            try {
                Thread.sleep(2000); // Sleep for 2 seconds
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }
}
