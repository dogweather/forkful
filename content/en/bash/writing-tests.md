---
title:    "Bash recipe: Writing tests"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Why

In the world of software development, writing tests is becoming an increasingly important aspect of the process. It serves as a way to ensure that your code is functioning as intended and helps catch any potential bugs or errors before they reach the end-user. Writing tests also promotes better code organization and improves overall code quality.

## How To

To write tests in Bash, we will be using a framework called "Bats". It provides a simple and easy-to-use syntax for writing tests in Bash scripts.

First, we need to install Bats on our system. On a Mac, we can do this using Homebrew:

```Bash
$ brew install bats
```

Once installed, we can start writing our tests. Let's say we have a simple Bash function that adds two numbers:

```Bash
add() {
  echo $(($1 + $2))
}
```

To test this function, we can create a new file called "test_add.sh" and add our test:

```Bash
#!/usr/bin/env bats

@test "add() should add two numbers" {
  result="$(add 5 3)"
  [ "$result" -eq 8 ]
}

@test "add() should handle negative numbers" {
  result="$(add -10 5)"
  [ "$result" -eq -5 ]
}
```

As you can see, Bats provides an intuitive syntax for writing tests. We use the "@test" keyword to define a new test and inside the curly braces, we can run our function and assert the expected result.

To run our tests, we can simply execute the "test_add.sh" file:

```Bash
$ ./test_add.sh
```

If all tests pass, we will see a green "ok" for each test. If any tests fail, Bats will provide a detailed error message.

## Deep Dive

Bats also allows us to use setup and teardown functions to perform actions before and after each test, respectively. For example, we can add a setup function to our previous example to initialize a variable that stores the expected result:

```Bash
@test "add() should handle negative numbers" {
  setup() {
    expected=-5
  }
  result="$(add -10 5)"
  [ "$result" -eq "$expected" ]
}
```

Furthermore, Bats also supports looping through tests using the "@for" keyword. This can be useful if we have a large number of similar tests that we want to run:

```Bash
@for number in 1 2 3 4 5; do
  @test "add() should double $number" {
    result="$(add "$number" "$number")"
    [ "$result" -eq "$(($number * 2))" ]
  }
done
```

For more advanced usage, Bats also offers features such as skipping certain tests, running tests in parallel, and capturing and checking output.

## See Also

- Bats documentation: https://github.com/bats-core/bats-core
- Unit Testing in Bash with Bats: https://medium.com/@itseranga/unit-testing-in-bash-using-bats-b6844ac599ec
- Bash Unit Testing: https://opensource.com/article/19/7/bash-testing