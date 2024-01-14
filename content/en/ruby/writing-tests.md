---
title:                "Ruby recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Writing tests in Ruby can seem like a tedious and time-consuming task, but it is an essential step in the development process. Tests help catch bugs early on, ensure code quality, and make refactoring easier. They also serve as documentation for your code, making it easier for other developers to understand and maintain it. In short, writing tests helps create a more robust and reliable application in the long run.

## How To

To write tests in Ruby, we will use the popular testing framework, RSpec. First, we need to install RSpec by running the following command in our terminal:

```
gem install rspec
```

To create a new test file, we use the `rspec --init` command. This will generate a `spec` folder and `spec_helper.rb` file in your project. Inside the `spec` folder, we will create a new file with the `.rb` extension. Let's name it `calculator_spec.rb`. This is where we will write our tests for a simple `Calculator` class.

```ruby
# spec/calculator_spec.rb
require "calculator"

RSpec.describe Calculator do
  describe "#add" do
    it "returns the sum of two numbers" do
      calculator = Calculator.new
      result = calculator.add(2, 3)

      expect(result).to eq(5)
    end
  end
end
```

In the code above, we first require our `Calculator` class, then define a `describe` block to group our tests. Within that, we have an `it` block to describe what our test is doing. In this case, we are testing the `add` method to ensure it returns the correct sum. Inside the `it` block, we create an instance of our `Calculator` class and call the `add` method with two numbers. Finally, we use the `expect` method to compare the result with our expected output using the `eq` matcher.

To run our test, we use the `rspec` command in our terminal, which will give us an output like this:

```ruby
$ rspec

Calculator
  #add
    returns the sum of two numbers

Finished in 0.00199 seconds (files took 0.17178 seconds to load)
1 example, 0 failures
```

## Deep Dive

When writing tests, it's important to keep in mind the different types of tests we can write. Unit tests are used to test individual units of code, such as methods or classes. Integration tests are used to test how different units work together. And acceptance tests are used to test the entire application from a user's perspective.

RSpec also provides different matchers to make our tests more flexible and readable. For example, we can use the `be_*` matcher to check for boolean values, the `change` matcher to monitor changes in objects, and the `have_*` matcher to check for specific attributes or methods.

Besides using the built-in matchers, we can also create custom matchers to make our tests more specific to our application's needs. This can help avoid repeating code and make our tests more readable.

It's also worth mentioning that while writing tests is important, we should also strive for quality code. Good code is easy to understand and test, which means writing clean, well-structured, and efficient code will make testing easier in the long run.

## See Also

- [RSpec documentation](https://rspec.info/)
- [Writing Effective Tests in Ruby](https://stackify.com/writing-good-ruby-tests/)