---
title:                "Writing tests"
html_title:           "Ruby recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests is a way for programmers to verify that their code works as intended. It involves creating a series of automated tests that simulate different use cases and check for expected outcomes. By writing tests, programmers can catch and fix bugs early on in the development process, ensuring that their code is reliable and functional.

## How to:
To write tests in Ruby, you'll need to use a testing framework such as MiniTest or RSpec. These frameworks provide methods and structure for writing tests in a clear and organized way. Here's an example of a simple test using MiniTest:

```Ruby
require 'minitest/autorun'

class TestCalculator < MiniTest::Test
  def setup
    @calculator = Calculator.new
  end

  def test_addition
    result = @calculator.add(2, 3)
    assert_equal(5, result)
  end
end

class Calculator
  def add(num1, num2)
    num1 + num2
  end
end
```

The test class is named `TestCalculator` and inherits from `MiniTest::Test`. The `setup` method creates an instance of our `Calculator` class to use in our test. The `test_addition` method defines our test case and uses the `assert_equal` method to check that the result of adding 2 and 3 is equal to 5.

Running this test would give us an output similar to this:

```
Run options: --seed 10568

# Running:

.

Finished in 0.000999s, 1001.0010 runs/s, 0.0000 assertions/s.

1 runs, 0 assertions, 0 failures, 1 errors, 0 skips
```

We can see that the test passed (indicated by the `.`) and we had 0 failures and 0 errors.

## Deep Dive:
The concept of writing tests is not unique to Ruby; it has been around for a long time and is a key aspect of the software development process. There are alternatives to testing frameworks such as using a debugger or manually testing code, but these methods are not as efficient or effective as automated testing.

In Ruby, there are two main types of testing frameworks: unit testing and integration testing. Unit testing focuses on testing individual pieces of code, while integration testing tests how multiple pieces of code work together. Writing tests in Ruby is made even easier with the use of tools like `minitest-rails` which integrates testing with the Rails framework.

## See Also:
- [MiniTest Docs](https://github.com/seattlerb/minitest)
- [RSpec](https://rspec.info/)
- [Rails Testing Guide](https://guides.rubyonrails.org/testing.html)