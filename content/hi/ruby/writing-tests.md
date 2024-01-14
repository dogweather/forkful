---
title:                "Ruby: टेस्ट दाखिल करना"
simple_title:         "टेस्ट दाखिल करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों

ऑब्जेक्ट-ओरिएंटेड प्रोग्रामिंग का उपयोग करके तेजी से बदलती दुनिया में, आपको अपने कोड को सुरक्षित और आसान बनाने के लिए टेस्ट लिखना अत्यंत आवश्यक है।

## कैसे करें

यहाँ हम Ruby में टेस्ट कैसे लिखते हैं, उसके बारे में सीखेंगे।

टेस्ट कोड लिखने से पहले, हमें आवश्यक गेम सेटअप को सेट करना होगा। इसके लिए, हमें `require 'minitest/autorun'` अभिव्यक्ति को एक फाइल के शुरुआत में जोड़नी होगी। यह आपको minitest लाइब्रेरी के साथ प्रोग्राम को लोड करना सुनिश्चित करता है।

अब, हमारे पास एक `Calculator` क्लास है जिसमे `add` और `subtract` डाला गया है। हमें सुनिश्चित करना होगा कि ये डाले गए ऑपरेशन्स सही तरीके से काम कर रहे हैं।

इसके लिए, हम निम्नलिखित रूप में टेस्ट कोड लिख सकते हैं:

```Ruby
require 'minitest/autorun'

class TestCalculator < Minitest::Test
  def setup
    @calc = Calculator.new
  end

  def test_add
    assert_equal 5, @calc.add(2, 3)
  end

  def test_subtract
    assert_equal 1, @calc.subtract(3, 2)
  end
end
```

जैसा कि आप देख सकते हैं, हमने `Minitest::Test` को अपने `TestCalculator` क्लास का एक उपयोग किया है। इससे, हमें `setup` अभिव्यक्ति को अपनी क्लास में जोड़ना होगा जो हर टेस्ट की सुरुआत में दोबारा उपयोग किए गए अभिव्यक्तियों को बनाता है।

फिर, हमें प्रत्येक `test` अभिव्यक्ति में एक `assert_equal` अभिव्यक्ति को लिखना होगा जो आशा करता है कि दी गई मूल्य परिणाम