---
title:                "Ruby recipe: Writing tests"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Why
Writing tests may seem like an extra step in the coding process, but it can actually be a valuable tool for any developer. By writing tests, you can ensure that your code is functioning correctly and prevent potential errors or bugs in the future.

## How To
To write tests in Ruby, you can use the built-in testing library called "minitest". Let's take a look at an example of a simple test:

```Ruby
require "minitest/autorun"

# Define a class to be tested
class Calculator
  def add(a, b)
    a + b
  end
end

# Define a test class
class TestCalculator < MiniTest::Test
  # Define a test for the add method
  def test_add
    # Create an instance of the Calculator class
    calculator = Calculator.new
    
    # Assert that the result of the add method is correct
    assert_equal 4, calculator.add(2, 2)
  end
end
```

In this example, we first require the "minitest/autorun" library which allows us to use the MiniTest::Test class for writing tests. Next, we define a class called Calculator which contains a method called "add" that adds two numbers together. Then, we create a test class called TestCalculator which inherits from MiniTest::Test and contains a test method called "test_add". Within this test method, we create an instance of the Calculator class and use the "assert_equal" assertion to ensure that the result of the add method is correct. If the result is not equal to 4, the test will fail.

To run this test, we can simply enter "ruby test_calculator.rb" in the terminal and it will output the test results. If all tests pass, you will see a dot for each test. If any tests fail, you will see an "F" and a failure message.

## Deep Dive
In addition to using the "assert_equal" assertion, there are other assertions you can use in your tests such as "assert", "refute_equal", and "refute". You can also use setup and teardown methods to set up any necessary data before each test and clean up after each test. It's important to write tests that cover all possible scenarios and edge cases to ensure that your code is functioning correctly.

Another helpful practice when writing tests is to use a code coverage tool, such as SimpleCov, to see which parts of your code are not being tested. This can help you identify any gaps in your test coverage and ensure that all of your code is being thoroughly tested.

## See Also
- [Ruby Testing for Beginners](https://rubyplus.com/articles/3571-How-to-Test-Ruby-Methods)
- [The Benefits of Test-Driven Development in Ruby](https://blog.testlodge.com/benefits-test-driven-development-ruby/)
- [Minitest Documentation](https://github.com/seattlerb/minitest)