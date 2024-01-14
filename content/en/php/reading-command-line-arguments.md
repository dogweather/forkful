---
title:    "PHP recipe: Reading command line arguments"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why

If you're new to programming or just starting out with PHP, you may be wondering why you would need to learn about reading command line arguments. Well, the answer is simple - it's a fundamental skill that can greatly enhance your coding abilities. By understanding how to read command line arguments, you'll have the ability to build more versatile and powerful applications, making you a more efficient and confident programmer.

## How To

First things first, let's go over the basic syntax for reading command line arguments in PHP:

```PHP
$arguments = getopt("f:t:", ["file:", "type:"]);

// Sample output
var_dump($arguments);

// Output:
array(2) {
  ["f"]=>
  string(10) "file1.txt"
  ["t"]=>
  string(4) "json"
}
```

In this example, we use the `getopt()` function to retrieve the command line arguments. The first argument of the function is a string of short options, in this case “f” and “t”. These are followed by an array of long options, in this case "file:" and "type:". The function returns an associative array with the arguments as keys and their corresponding values.

Next, let's look at an example of how to use this in a practical application. Let's say you want to write a script that converts a file from one type to another. With the knowledge of reading command line arguments, you can easily specify the input file and output file types in the command line:

```PHP
// Convert file from CSV to JSON
php convert.php -f input.csv -t json

// Convert file from XML to YAML
php convert.php --file=input.xml --type=yaml
```

By using this method, you can easily manipulate and manipulate command line arguments to suit your specific needs without having to modify your code every time.

## Deep Dive

Now that you understand the basics of reading command line arguments, let's dive deeper into how this function works. The `getopt()` function uses the `argv` array, which stores all the arguments passed to the script. This array can be accessed directly, but using the `getopt()` function makes it easier to handle and manipulate the arguments.

In addition, the `getopt()` function also offers various options for handling specific situations, such as handling required arguments or conflicting options. By exploring these options and using them in your code, you can create a more efficient and robust application.

## See Also

- [PHP.net - Command Line Options](https://www.php.net/manual/en/function.getopt.php)
- [Stack Overflow - How to Read Command Line Arguments in PHP](https://stackoverflow.com/questions/29322634/how-to-read-command-line-arguments-in-php)
- [Medium - Mastering Command Line Arguments in PHP](https://medium.com/@aequasi/php-tips-mastering-command-line-arguments-15d265d0930f)