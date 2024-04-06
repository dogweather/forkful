---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:02.327205-07:00
description: "\u0915\u0948\u0938\u0947: Ruby \u092E\u0947\u0902 `Test::Unit` \u0928\
  \u093E\u092E\u0915 \u090F\u0915 \u0928\u093F\u0930\u094D\u092E\u093F\u0924 \u092A\
  \u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F \u0939\u0948 \u091C\u094B \u0907\
  \u0915\u093E\u0908 \u092A\u0930\u0940\u0915\u094D\u0937\u0923\u094B\u0902 \u0915\
  \u094B \u0932\u093F\u0916\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0939\u0948\
  , \u091C\u094B \u0938\u0930\u0932 \u0938\u0902\u0930\u091A\u0928\u093E\u0913\u0902\
  \ \u0915\u0947 \u092D\u0940\u0924\u0930 \u092A\u0930\u0940\u0915\u094D\u0937\u0923\
  \ \u092A\u094D\u0930\u0925\u093E\u0913\u0902 \u0915\u094B \u0938\u092E\u093E\u0939\
  \u093F\u0924 \u0915\u0930\u0924\u0940 \u0939\u0948\u0964\u2026"
lastmod: '2024-04-05T21:53:55.186132-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u092E\u0947\u0902 `Test::Unit` \u0928\u093E\u092E\u0915 \u090F\u0915\
  \ \u0928\u093F\u0930\u094D\u092E\u093F\u0924 \u092A\u0941\u0938\u094D\u0924\u0915\
  \u093E\u0932\u092F \u0939\u0948 \u091C\u094B \u0907\u0915\u093E\u0908 \u092A\u0930\
  \u0940\u0915\u094D\u0937\u0923\u094B\u0902 \u0915\u094B \u0932\u093F\u0916\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F \u0939\u0948, \u091C\u094B \u0938\u0930\u0932\
  \ \u0938\u0902\u0930\u091A\u0928\u093E\u0913\u0902 \u0915\u0947 \u092D\u0940\u0924\
  \u0930 \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u092A\u094D\u0930\u0925\u093E\
  \u0913\u0902 \u0915\u094B \u0938\u092E\u093E\u0939\u093F\u0924 \u0915\u0930\u0924\
  \u0940 \u0939\u0948\u0964 \u0939\u093E\u0932\u093E\u0902\u0915\u093F, Ruby \u0938\
  \u092E\u0941\u0926\u093E\u092F \u0905\u0915\u094D\u0938\u0930 \u0909\u0928\u0915\
  \u0940 \u0909\u0928\u094D\u0928\u0924 \u0905\u092D\u093F\u0935\u094D\u092F\u0915\
  \u094D\u0924\u093F \u0914\u0930 \u0932\u091A\u0940\u0932\u093E\u092A\u0928 \u0915\
  \u0947 \u0915\u093E\u0930\u0923 RSpec \u0914\u0930 Minitest \u091C\u0948\u0938\u0940\
  \ \u0924\u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092A\u0941\u0938\u094D\
  \u0924\u0915\u093E\u0932\u092F\u094B\u0902 \u0915\u0940 \u0913\u0930 \u091D\u0941\
  \u0915\u093E\u0935 \u0930\u0916\u0924\u093E \u0939\u0948\u0964."
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
weight: 36
---

## कैसे:
Ruby में `Test::Unit` नामक एक निर्मित पुस्तकालय है जो इकाई परीक्षणों को लिखने के लिए है, जो सरल संरचनाओं के भीतर परीक्षण प्रथाओं को समाहित करती है। हालांकि, Ruby समुदाय अक्सर उनकी उन्नत अभिव्यक्ति और लचीलापन के कारण RSpec और Minitest जैसी तृतीय-पक्ष पुस्तकालयों की ओर झुकाव रखता है।

### `Test::Unit` का उपयोग करते हुए:
```ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    result = 2 + 2
    assert_equal 4, result
  end
end
```
अपनी परीक्षण फ़ाइल को टर्मिनल से चलाएं, और आपको एक आउटपुट मिलना चाहिए जो परीक्षणों की सफलता या विफलता को इंगित करता है:
```
Loaded suite test_calculator
Started
.
Finished in 0.001288 seconds.
1 tests, 1 assertions, 0 failures, 0 errors, 0 pendings, 0 omissions, 0 notifications
100% passed
```

### RSpec का उपयोग करते हुए:
RSpec Ruby के लिए एक लोकप्रिय BDD (Behavior-Driven Development) ढांचा है। `gem install rspec` के साथ इसे स्थापित करें, फिर इसे अपने प्रोजेक्ट में `rspec --init` के साथ प्रारंभ करें।

```ruby
# calculator_spec.rb
require_relative '../calculator'

describe Calculator do
  it 'सही तरीके से दो संख्याओं को जोड़ती है' do
    expect(Calculator.add(2, 2)).to eq(4)
  end
end
```
`rspec` कमांड के साथ परीक्षण चलाएं। उदाहरण आउटपुट:
```
.

Finished in 0.002 seconds (files took 0.1 seconds to load)
1 example, 0 failures
```

### Minitest का उपयोग करते हुए:
Minitest TDD, BDD, मॉकिंग, और बेंचमार्किंग का समर्थन करने वाली परीक्षण सुविधाओं का एक पूरा सेट प्रदान करता है। इसे `gem install minitest` के साथ स्थापित करें और इस प्रकार इसका उपयोग करें:

```ruby
# test_calculator.rb
require 'minitest/autorun'
require_relative '../calculator'

class CalculatorTest < Minitest::Test
  def test_addition
    assert_equal 4, Calculator.add(2, 2)
  end
end
```

अपनी परीक्षण फ़ाइल को सीधे या minitest के लिए सेट किए गए `rake` टास्क के माध्यम से चलाएं। नमूना आउटपुट:
```
Run options: --seed 33407

# Running:

.

Finished in 0.001027s, 974.5922 runs/s, 974.5922 assertions/s.
1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

इन पुस्तकालयों का उपयोग करके अपने Ruby परियोजनाओं में परीक्षण को लागू करके, आप इससे अधिक विश्वसनीय और बनाए रखने योग्य कोड आधारों की ओर ले जाते हैं।
