Perceptron: neuron.hs Perceptron.c
	ghc -c -O2 neuron.hs
	ghc --make -no-hs-main -optc-O2 Perceptron.c neuron.hs -o Perceptron
	@echo "Compiled."

clean:
	rm -f *~ *.o *.hi