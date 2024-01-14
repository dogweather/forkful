---
title:    "Fish Shell recipe: Searching and replacing text"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why
Text editing can be a tedious task, especially when you have a large amount of text to deal with. But fear not, with the power of Fish Shell, you can make this process much more efficient by using the search and replace function.

## How To
To search and replace text using Fish Shell, follow these simple steps:

1. Open your terminal and navigate to the directory where the text file is located.
2. Use the `sed` command along with the search and replace function syntax. For example, if you want to replace all instances of "hello" with "hi" in a text file called "greetings.txt", you would use the following code:

   ```Fish Shell
   sed -i 's/hello/hi/g' greetings.txt
   ```

   Let's break down this command:

   - `sed`: This is the command used for stream editing and can be used to perform various operations on text files.
   - `-i`: This flag is used to edit the file in-place, meaning the changes will be made directly to the original file.
   - `s/`: This is the substitute command, which is used to search and replace text.
   - `/hello/`: This is the pattern we want to search for. You can replace it with any text or regular expression.
   - `/hi/`: This is the replacement text. You can replace it with any text of your choice.
   - `/g`: This is the global flag, which ensures that all instances of the search pattern are replaced.

3. Press enter and your text will be automatically updated with the changes.

## Deep Dive
The search and replace function in Fish Shell is not limited to simple text replacements. It also supports regular expressions, which allows for more advanced and complex search patterns.

For example, if you want to replace all numbers in a text file with the word "number", you can use the following code:

```Fish Shell
sed -i 's/[0-9]+/number/g' numbers.txt
```

Let's break down this regular expression:

- `[0-9]+`: This is the pattern we want to search for, which represents one or more digits.
- `/number/`: This is the replacement text, which will replace all numbers with the word "number".
- `/g`: This global flag ensures that all instances of the search pattern are replaced, not just the first one.

You can also use the search and replace function in conjunction with other Fish Shell commands, such as `grep` and `cat`, to further manipulate and edit your text files.

## See Also
- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Sed documentation](https://www.gnu.org/software/sed/manual/sed.html)
- [Regular Expressions tutorial](https://www.regular-expressions.info/tutorial.html)