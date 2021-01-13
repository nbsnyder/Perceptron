#include <stdio.h>
#include <stdlib.h>
#include <HsFFI.h>



// Helper function for the Haskell code to read a value from an array of doubles
double readFromDblArr(double* arr, int index) {
    return arr[index];
}

// Helper function for the Haskell code to write a value to an array of doubles
void writeToDblArr(double* arr, int index, double val) {
    arr[index] = val;
}

/*
Find the optimal weights given input and output arrays and write them to the weights array
Defined in neuron.hs
Parameters: (1) the inputs array (double*), (2) the weights array (double*), (3) the outputs array (double*)
Parameters (cont.): (4) the major length of the inputs array, (5) the minor length of the inputs array
*/
extern void getWeights(HsPtr a1, HsPtr a2, HsPtr a3, HsInt32 a4, HsInt32 a5);



int main(int argc, char **argv) {
    int size1 = 0, size1Max = 8, size2 = 3;
    double *inputs, *outputs, *weights;
    FILE *file1;

    if (argc != 2) {
        printf("Error: Invalid arguments\n");
        return -1;
    }

    file1 = fopen(argv[1], "r");

    if (file1 == NULL) {
        printf("Error: No such file or directory\n");
        return -1;
    }

    inputs = (double*) malloc(size1Max * size2 * sizeof(double));
    outputs = (double*) malloc(size1Max * sizeof(double));
    weights = (double*) malloc(size2 * sizeof(double));

    while (fscanf(file1, "%lf %lf %lf\n", &inputs[(size1 * size2) + 1], &inputs[(size1 * size2) + 2], &outputs[size1]) > 0) {
        inputs[size1 * size2] = 1.0;
        size1++;
        if (size1 == size1Max) {
            size1Max *= 2;
            inputs = realloc(inputs, size1Max * size2 * sizeof(double));
            outputs = realloc(outputs, size1Max * size2 * sizeof(double));
        }
    }

    fclose(file1);

    hs_init(&argc, &argv);
    getWeights(inputs, weights, outputs, size1, size2);
    hs_exit();

    printf("Number of inputs: %d. Max size of the array: %d\n\n", size1, size1Max);

    printf("Weights: [");
    for (int i = 0; i < size2; i++) printf("%lf ", weights[i]);
    printf("\b]\n");

    free(inputs);
    free(outputs);
    free(weights);

    return 0;
}
