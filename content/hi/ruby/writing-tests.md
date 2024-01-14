---
title:                "Ruby: टेस्ट लेखन"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों
प्रोग्रामिंग में टेस्ट कोड लिखने का सबसे महत्वपूर्ण कारण है कि यह हमें चाहिए ताकि हम अपनी कोड को ठीक ढंग से चलाने और सुनिश्चित कर सकें कि यह सही तरीके से काम कर रहा है। टेस्ट कोड लिखने से हम इसकी परीक्षण और सुरक्षा में भी सुधार कर सकते हैं।

## कैसे करें
टेस्ट कोड लिखने के लिए हम `Minitest` या `RSpec` की मदद ले सकते हैं। सबसे पहले हम आगे बढ़ने से पहले `gem install` कमांड के द्वारा अपने सिस्टम में `Minitest` या `RSpec` को इंस्टॉल कर सकते हैं। अगर आप `Ruby on Rails` में काम कर रहे हैं तो आपको इन को अपने `Gemfile` में भी जोड़ना होगा।

टेस्ट कोड को लिखने के लिए हमें सामान्य फ़ाइल `test/test_file_name.rb` को बनाना होगा। यहां हम आपको एक उदाहरण दे रहे हैं।

```Ruby
# test/calculator_test.rb
require "minitest/autorun"
require_relative "../calculator"

class CalculatorTest < Minitest::Test
  def setup
    @calculator = Calculator.new
  end

  def test_add
    assert_equal 5, @calculator.add(2, 3), "अनुमानित परिणाम 5 होना चाहिए लेकिन असली परिणाम #{@calculator}"
  end

  def test_subtract
    assert_equal 2, @calculator.subtract(5, 3), "अनुमानित परिणाम 2 होना चाहिए लेकिन असली परिणाम #{@calculator}"
  end
end
```

सामान्य स्थितियों में हम `assert_equal` में दो अर्ग्युमेंट पर्याप्त होते हैं: आपकी अनुमानित परिणाम और आपकी तय की गई सीमा। यदि अनुमानित परिणाम और असली परिणाम में कोई अंतर नहीं है, तो टेस्ट पास हो जाएगा। अन्यथा, यह विफल हो जाएगा और पास किए गए आउटपुट में