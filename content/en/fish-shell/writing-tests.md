---
title:                "Fish Shell recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Writing tests is an important aspect of programming, regardless of the language or framework being used. It allows developers to catch bugs and errors early on in the development process, ensuring a more stable and reliable application. By investing time in writing tests, you can save yourself from hours of debugging in the future.

## How To

Writing tests in Fish Shell is a breeze with the `test` command. Let's look at an example of testing a simple function that adds two numbers together.

````Fish Shell
function add_numbers
  echo "Enter first number"
  read num1
  echo "Enter second number"
  read num2
  echo ($num1 + $num2)
end
````

In the above code, we have a function `add_numbers` that takes two user input numbers and adds them together before returning the result. Now, let's write a test for this function.

````Fish Shell
function test_add_numbers
  test (add_numbers 5 5) = 10
  or echo "Addition test failed!"
end
````

The `test` command takes in an expression and checks if it evaluates to `true`. If it does, the test passes. In the above code, we are passing the numbers 5 and 5 to the `add_numbers` function and checking if it returns 10, which it should. We also have an `or` statement to display an error message in case the test fails.

To run this test, we need to execute the `test_add_numbers` function. We can do so by calling it in our terminal:

````Fish Shell
test_add_numbers
````

If the test passes, nothing will be displayed. However, if the test fails, the error message we specified will be shown. It's as simple as that!

## Deep Dive

Fish Shell also has the ability to define multiple test sections for a single function. This allows you to have different test cases for different scenarios. Let's add a conditional statement to our `add_numbers` function and see how we can write multiple tests for it.

````Fish Shell
function add_numbers
  echo "Enter first number"
  read num1
  echo "Enter second number"
  read num2

  if test $num1 -gt $num2
    echo "First number is larger"
  else if test $num1 -lt $num2
    echo "Second number is larger"
  else
    echo "Both numbers are equal"
  end

  echo ($num1 + $num2)
end
````

Now, we can write three separate tests for each possible outcome of the function:

````Fish Shell
function test_add_numbers_greater
  test (add_numbers 10 5) = 15
  or echo "Greater test failed!"
end

function test_add_numbers_less
  test (add_numbers 5 10) = 15
  or echo "Less test failed!"
end

function test_add_numbers_equal
  test (add_numbers 5 5) = 10
  or echo "Equal test failed!"
end
````

By writing multiple tests, we can cover all possible scenarios and ensure our function works as expected in any situation.

## See Also

- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Writing Tests in Fish Shell](https://fishshell.com/docs/current/tutorial.html#writing-tests)
- [Introduction to Fish Shell](https://opensource.com/article/20/3/fish-shell)