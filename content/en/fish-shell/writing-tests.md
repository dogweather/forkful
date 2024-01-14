---
title:                "Fish Shell recipe: Writing tests"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

# Why: The Importance of Writing Tests

When writing code, it's crucial to ensure its functionality and catch any potential bugs or errors that may arise. This is where writing tests comes in handy. By creating tests, you can quickly and easily check your code's performance and catch any issues before they reach production. Writing tests also allows for easier collaboration with other developers by providing a clear understanding of expected outcomes.

# How To: Writing Tests in Fish Shell

In Fish Shell, you can write tests using the `test` function. This function takes in two arguments: a condition and a description. Here's an example of a simple test that checks if a variable is equal to a specific value:

```Fish Shell
test $var = "Hello World" "Check if var is equal to Hello World"
```

To run this test, simply type `fish -t filename.fish`, and you will see the following output:

```Terminal
> Check if var is equal to Hello World
```

If the test fails, you will see an error message detailing the reason for the failure. It's essential to provide descriptive descriptions for your tests to easily identify which tests have failed.

You can also use the `not` keyword to check for the opposite condition. For example,

```Fish Shell
test not (count $string) = 0 "Check if string is not empty"
```

Lastly, you can use the `contains` function to check if a string contains a particular substring.

```Fish Shell
test contains "Fish" "Checking if this sentence contains the word Fish"
```

# Deep Dive: Tips for Writing Effective Tests

- Use clear and descriptive names for your tests to easily identify their purpose.
- Organize your tests by category or function for better organization and readability.
- Write tests for both expected and unexpected inputs to catch any edge cases.
- Make use of the `not` keyword to check for negative conditions.
- Utilize the `eq` function for more precise comparisons.
- Write tests while you code to catch errors early on and save time in the long run.

# See Also

For more information on writing tests in Fish Shell, check out the following resources:

- [Official Fish Shell Documentation on Testing](https://fishshell.com/docs/current/cmds/test.html)
- [Guide to Writing Tests in Fish Shell by Thoughtbot](https://thoughtbot.com/blog/writing-a-test-suite-in-fish-shell)
- [Tutorial on Writing Automated Tests in Fish Shell by Medium](https://medium.com/swlh/testing-frameworks-in-fish-shell-f1dca2f522b8)

Remember, writing tests is an invaluable tool that can greatly improve your coding process. So start implementing tests in your Fish Shell scripts today for better and more reliable code. Happy testing!