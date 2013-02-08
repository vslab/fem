build/Distributions.dll: build/RandomTools.dll distr/RandomVariables.fs distr/Distributions.fs
	fsc -r build/RandomTools.dll --target:library --out:build/Distributions.dll distr/RandomVariables.fs distr/Distributions.fs

build/RandomTools.dll: RandomTools/RandomTools.cs RandomTools/Mersenne.cs
	mcs -t:library -out:build/RandomTools.dll RandomTools/RandomTools.cs RandomTools/Mersenne.cs

