#include <iostream>
#include <stack>
#include <vector>

using namespace std;

// Function to perform partitioning of the array
int partition(vector<int>& arr, int low, int high) {
    int pivot = arr[high];  // Choose last element as pivot
    int i = low - 1;  // Initialize index of smaller element

    // Traverse through array
    for (int j = low; j < high; ++j) {
        // If current element is less than or equal to the pivot, then do this
        if (arr[j] <= pivot) {
            ++i;  // Move the smaller element index
            swap(arr[i], arr[j]);  // Swap the elements
        }
    }

    // Place the pivot element in correct position
    swap(arr[i + 1], arr[high]);
    
    // Return the partition index 
    return i + 1;
}

// Iterative version of QuickSort using a stack 
void quicksort(vector<int>& arr, int low, int high) {
    stack<pair<int, int>> s;  // Stack simulates recursive calls
    s.push({low, high});  // Push the initial range to the stack
    
    // Loop until there are no more ranges to process
    while (!s.empty()) {
        pair<int, int> p = s.top();  // Get the current range
        s.pop();  // Pop the range
        
        int l = p.first;  // Start of the range
        int h = p.second;  // End of the range
        
        // If the range has more than one element
        if (l < h) {
            // Partition the array and get the pivot index
            int pivot = partition(arr, l, h);
            
            // Push the two subranges (left and right of pivot) to the stack
            s.push({l, pivot - 1});  // Left subrange
            s.push({pivot + 1, h});  // Right subrange
        }
    }
}

int main() {
    // Sample array to be sorted
    vector<int> arr = {120, 10, 300, 450, 402, 20, 70};
    
    // Call quicksort function
    quicksort(arr, 0, arr.size() - 1);
    
    // Output sorted array
    for (int num : arr) {
        cout << num << " ";  // Print each number separated by a space
    }
    cout << endl;  // Print a newline
    
    return 0;  
}
