---
title:    "Bash recipe: Generating random numbers"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why

As a programmer, you may have encountered scenarios where you need to generate random numbers. This could be for various reasons such as creating randomized test data, implementing a game or simulation, or for security purposes. Regardless of the reason, knowing how to generate random numbers in Bash can be a useful skill to have in your arsenal.

## How To

To generate random numbers in Bash, you can make use of the built-in 'shuf' command. This command allows you to shuffle and output random lines from a file or a range of numbers.

To generate a single random number between a specific range, you can use the following code:

```Bash
echo $(( RANDOM % (max - min + 1) + min ))
```
In this code, 'max' and 'min' represent the maximum and minimum values of the range, respectively. The 'RANDOM' variable generates a random number between 0 and 32767, and the modulus operator (%) helps in keeping the number within the range.

For example, if you want to generate a random number between 1 and 10, you can use:
```Bash
echo $(( RANDOM % (10 - 1 + 1) + 1 ))
```

To generate multiple random numbers within a range, you can use a 'for' loop and keep the 'min' and 'max' values as variables. Here's an example to generate five random numbers between 1 and 100:

```Bash
min=1
max=100
for i in {1..5}
do
  echo $(( RANDOM % (max - min + 1) + min ))
done
```

The output of this code would be five random numbers between 1 and 100, each on a separate line.

## Deep Dive

The 'RANDOM' variable generates a pseudorandom number, meaning it follows a predetermined sequence. This sequence may not be truly random and can be predicted after a certain number of values. If you require genuinely random numbers, you can make use of external tools such as 'openssl' or 'gpg' to generate random numbers based on hardware noise.

Additionally, you can also use the 'dev/urandom' device file in Linux to generate random numbers. This file utilizes chaotic processes in the operating system to generate seemingly random numbers.

## See Also

- [Linuxize: Random Number Generation in Bash](https://linuxize.com/post/bash-random-number/)
- [Gnu.org: Bash Random Numbers](https://www.gnu.org/software/bash/manual/html_node/Rand-Example.html)
- [Bash Hackers Wiki: Random Numbers](http://wiki.bash-hackers.org/commands/builtin/rand)

By now, you should have a good understanding of how to generate random numbers in Bash and some options for getting truly random numbers. Experiment with these methods and find the one that works best for your specific use case.