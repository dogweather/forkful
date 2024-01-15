---
title:                "Writing tests"
html_title:           "Bash recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Writing tests is an essential part of software development, ensuring that the code you write functions as expected. By having tests in place, you can catch and fix any bugs or errors early on in the development process, saving time and effort in the long run.

## How To

To get started with writing tests in Bash, you first need to have a basic understanding of the language and its syntax. Once you have that, you can follow these steps to write your first test:

1. Identify the piece of code you want to test. This could be a function, a script, or a command.
2. Create a new file and name it something like `test_yourcode.sh`.
3. Use the `source` command to include the file containing the code you want to test.
4. Declare your tests inside the `test_yourcode.sh` file, using the `assert` keyword to check if the expected output matches the actual output.
5. Run the `test_yourcode.sh` file using the `bash` command in your terminal.
6. Check the output of the tests to see if they pass or fail. If there are any failures, go back and debug your code until all tests pass.

Let's look at an example of testing a simple `add` function:

```Bash
# add function
add() {
  echo $(( $1 + $2 ))
}

source add.sh # assuming this file contains the add function

# tests for add function
assert "add 5 2" "7" # should pass
assert "add 10 0" "11" # should fail
```

Running the `test_add.sh` file would give the following output:

```
1 out of 2 tests passed.
Test 2 failed: Expected 11, got 10.
```

From this output, we can see that our first test passed, but the second test failed. This indicates that there is an error in our `add` function, which we can then go back and fix.

## Deep Dive

Writing tests in Bash involves using the `source` command to include the code you want to test, and then using the `assert` keyword to compare the expected output with the actual output. You can also use other keywords such as `assert_exit` to check the exit status of a command and `assert_equal` to compare two variables.

It is important to note that when writing tests in Bash, you should ensure that your code is clean and well-organized. This will make it easier to identify and fix any errors that may arise during the testing process.

## See Also

- [Bash Official Documentation](https://www.gnu.org/software/bash/)
- [Bash Beginner's Guide](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Shell Scripting Tutorial](https://www.shellscript.sh/)