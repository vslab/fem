using Generators;
using System;
using System.Linq;
using System.Security.Cryptography;
namespace RandomTools
{
    public class RandomSource
    {
        static private RandomNumberGenerator bigGenerator = RandomNumberGenerator.Create();
        private MersenneTwister generator;
        private int[] seed;
        private int counter;
        private double[] cache;

        private void GenNextSample()
        {
            cache[++counter & (cache.Length - 1)] = generator.NextDouble();
        }

        private void Reset()
        {
            counter = 0;
            generator = new MersenneTwister(seed);
            cache[0] = generator.NextDouble();
        }

        private void Realloc()
        {
            cache = new double[cache.Length * 2];
        }

        public RandomSource()
        {
            var newSeed = new byte[MersenneTwister.maxSeedSize];
            var newSeed2 = new int[newSeed.Length / 4];
            bigGenerator.GetBytes(newSeed);
            for (var i = 0; i < newSeed2.Length; i++)
            {
                newSeed2[i] = newSeed[i * 4 + 0] << 24
                            + newSeed[i * 4 + 1] << 16
                            + newSeed[i * 4 + 2] << 8
                            + newSeed[i * 4 + 3] << 0;
            }
            Init(newSeed2);
        }

        private void Init (System.Collections.Generic.IEnumerable<int> inSeed)
        {
            seed =  inSeed.ToArray();
            cache = new double[1];
            Reset();
        }

        public RandomSource(System.Collections.Generic.IEnumerable<int> inSeed)
        {
            Init(inSeed);
        }

        public double GetSample(int seqNumber)
        {
            //check for cache miss, if so reset and double cache size
            if (counter >= seqNumber + cache.Length)
            {
                if (seqNumber + 1 >= cache.Length)
                    Realloc();
                Reset();
            }
            //generates samples up to the number required
            while (counter < seqNumber)
                GenNextSample();

            //return sample from cache
            return cache[seqNumber & (cache.Length - 1)];
        }
    };
}
