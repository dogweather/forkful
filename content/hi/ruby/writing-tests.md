---
title:                "परीक्षण लिखना"
date:                  2024-01-19
simple_title:         "परीक्षण लिखना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
परीक्षण लेखन का मतलब है कोड की उच्चता और कार्यकुशलता जांचना। प्रोग्रामर इसे बग्स कम करने, और सॉफ्टवेयर की स्थिरता और विश्वसनीयता सुनिश्चित करने के लिए करते हैं।

## How to: (कैसे करें:)
```Ruby
# test_example.rb
require 'minitest/autorun'

class Calculator
  def add(a, b)
    a + b
  end
end

class TestCalculator < Minitest::Test
  def test_addition
    calculator = Calculator.new
    assert_equal 5, calculator.add(2, 3)
  end
end
```
आउटपुट:
```
Run options: --seed 12345

# Running:

.

Finished in 0.001014s, 985.2213 runs/s, 985.2213 assertions/s.

1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

## Deep Dive (गहराई में जानकारी)
रूबी में टेस्टिंग लाइब्रेरीज की शुरुआत हुई with Test::Unit, और फिर उसे Minitest और RSpec ने ले लिया। आजकल, RSpec बहुत लोकप्रिय है पर Minitest का इस्तेमाल सादगी और तेज़ी के लिए होता है। टेस्ट केसेस को चलते वक्त, वे अस्वीकरण (assertions) का इस्तेमाल करते हैं तथा अपेक्षित और वास्तविक परिणामों की तुलना करते हैं।

## See Also (और जानकारी के लिए)
- [RSpec Documentation](https://rspec.info/)
- [RubyGuides - An Introduction to Ruby Tests](https://www.rubyguides.com/2018/07/rspec-tutorial/)
- [Ruby Testing for Beginners](https://www.youtube.com/watch?v=71eKcNxwxVY)
