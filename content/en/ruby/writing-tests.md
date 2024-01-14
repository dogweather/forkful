---
title:    "Ruby recipe: Writing tests"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Why

When it comes to writing code, it's important to ensure that it works correctly and doesn't break unexpectedly. This is where testing comes in. Writing tests allows developers to check their code for errors and bugs, ensuring the quality and functionality of their code.

## How To

To write tests in Ruby, we will be using a popular testing framework called RSpec. First, we need to install RSpec using the following command:

```Ruby
gem install rspec
```

Next, we can create a new Ruby file for our tests. Let's name it `calculator_spec.rb`. In this file, we will require the `rspec` library and the file containing the code we want to test. Our `calculator_spec.rb` file should look like this:

```Ruby
require 'rspec'
require_relative 'calculator'

# tests will go here
```

Now, let's write a simple test to make sure our `Calculator` class can add two numbers correctly. We will use the `describe` and `it` methods provided by RSpec to group and label our tests. Our test should look like this:

```Ruby
describe Calculator do
  describe "add" do
    it "correctly adds two numbers" do
      # instantiate a new Calculator object
      calculator = Calculator.new
      # assign the result of the add method to a variable
      result = calculator.add(2, 3)
      # use the expect method to check if the result is equal to 5
      expect(result).to eq(5)
    end
  end
end
```

We can now run our tests by executing the `rspec` command in our terminal. If our test passes, we should see an output similar to the following:

```Ruby
.

Finished in 0.002 seconds (files took 0.24816 seconds to load)
1 example, 0 failures
```

The `.` indicates that our test has passed successfully.

## Deep Dive

Writing tests not only helps us catch and fix errors, but it also allows us to design our code in a more organized and logical manner. When writing tests, it's important to think about different scenarios and edge cases to ensure our code is robust and can handle various inputs.

In addition, writing tests can also serve as documentation for our code. Tests act as live examples of how our code should be used and can help other developers understand our code more easily.

In terms of best practices, it's recommended to write tests for every new feature or bug fix. Also, it's important to keep tests independent of each other to avoid any dependencies and to make sure each test can be run individually.

## See Also

For more information on RSpec and writing tests in Ruby, check out the following links:

- [RSpec website](https://rspec.info/)
- [RubyDocs: RSpec](https://rubydoc.info/gems/rspec/)
- [Better Specs: A RSpec Style Guide](https://www.betterspecs.org/)