---
title:                "Writing tests"
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests checks if code works as expected. Programmers do it to catch bugs early, ensure reliability, and ease future code changes.

## How to:

Ruby uses Minitest and RSpec for testing—let's use RSpec. First, install it:

```ruby
gem install rspec
```

Create a test file, `calculator_spec.rb`:

```ruby
RSpec.describe Calculator do
  describe "#add" do
    it "sums two numbers" do
      expect(Calculator.new.add(3, 7)).to eql(10)
    end
  end
end
```

Run the test with:

```shell
rspec calculator_spec.rb
```

Output:

```
F

Failures:

  1) Calculator#add sums two numbers
     Failure/Error: expect(Calculator.new.add(3, 7)).to eql(10)
     
     NameError:
       uninitialized constant Calculator
```

Create `calculator.rb`:

```ruby
class Calculator
  def add(a, b)
    a + b
  end
end
```

Run tests again.

Output:

```
.

Finished in 0.002 seconds (files took 0.08 seconds to load)
1 example, 0 failures
```

## Deep Dive

Testing in Ruby goes back to Test::Unit, but RSpec, introduced in 2005, revolutionized Ruby testing with "behavior-driven development". Alternatives to RSpec include Minitest and Test::Unit. RSpec focuses on readability and the business side; Minitest is more minimalist and faster. Typically, tests mimic software use, checking functions, data, and edge cases. For existing projects, start by testing the most critical parts.

## See Also

- RSpec GitHub: [github.com/rspec/rspec](https://github.com/rspec/rspec)
- Minitest: [rubygems.org/gems/minitest](https://rubygems.org/gems/minitest)
- "Effective Testing with RSpec 3": Read for more on RSpec principles and patterns.
