---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:55.887568-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF \u0987\
  \u0989\u09A8\u09BF\u099F \u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09C7\u0996\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF `Test::Unit` \u09A8\u09BE\u09AE\u0995 \u098F\u0995\
  \u099F\u09BF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF \u09B8\u09BE\u09A5\u09C7 \u0986\u09B8\u09C7, \u09AF\
  \u09BE \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982 \u09AA\u09CD\u09B0\u09CD\u09AF\
  \u09BE\u0995\u099F\u09BF\u09B8\u0997\u09C1\u09B2\u09CB\u0995\u09C7 \u09B8\u09B0\u09B2\
  \ \u0995\u09BE\u09A0\u09BE\u09AE\u09CB\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u0986\
  \u09AC\u09A6\u09CD\u09A7 \u0995\u09B0\u09C7\u0964 \u09A4\u09AC\u09C7, \u09B0\u09C1\
  \u09AC\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.594017-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF \u0987\u0989\u09A8\u09BF\u099F \u099F\u09C7\u09B8\
  \u09CD\u099F \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF `Test::Unit`\
  \ \u09A8\u09BE\u09AE\u0995 \u098F\u0995\u099F\u09BF \u09AC\u09BF\u09B2\u09CD\u099F\
  -\u0987\u09A8 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09B8\u09BE\
  \u09A5\u09C7 \u0986\u09B8\u09C7, \u09AF\u09BE \u099F\u09C7\u09B8\u09CD\u099F\u09BF\
  \u0982 \u09AA\u09CD\u09B0\u09CD\u09AF\u09BE\u0995\u099F\u09BF\u09B8\u0997\u09C1\u09B2\
  \u09CB\u0995\u09C7 \u09B8\u09B0\u09B2 \u0995\u09BE\u09A0\u09BE\u09AE\u09CB\u09B0\
  \ \u09AE\u09A7\u09CD\u09AF\u09C7 \u0986\u09AC\u09A6\u09CD\u09A7 \u0995\u09B0\u09C7\
  \u0964 \u09A4\u09AC\u09C7, \u09B0\u09C1\u09AC\u09BF \u0995\u09AE\u09BF\u0989\u09A8\
  \u09BF\u099F\u09BF \u09AA\u09CD\u09B0\u09BE\u09DF\u0987 RSpec \u098F\u09AC\u0982\
  \ Minitest \u098F\u09B0 \u09AE\u09A4 \u09A4\u09C3\u09A4\u09C0\u09DF \u09AA\u0995\
  \u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\
  \u09C1\u09B2\u09CB\u09B0 \u09A6\u09BF\u0995\u09C7 \u099D\u09C1\u0981\u0995\u09C7\
  \ \u09A5\u09BE\u0995\u09C7 \u0995\u09BE\u09B0\u09A3 \u098F\u0997\u09C1\u09B2\u09CB\
  \u09B0 \u09AC\u09CD\u09AF\u0995\u09CD\u09A4\u09BF\u0997\u09A4 \u09AA\u09CD\u09B0\
  \u0995\u09BE\u09B6\u09AD\u0999\u09CD\u0997\u09BF \u098F\u09AC\u0982 \u09A8\u09AE\
  \u09A8\u09C0\u09DF\u09A4\u09BE \u09AC\u09C7\u09B6\u09BF\u0964\n"
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কিভাবে:
রুবি ইউনিট টেস্ট লেখার জন্য `Test::Unit` নামক একটি বিল্ট-ইন লাইব্রেরি সাথে আসে, যা টেস্টিং প্র্যাকটিসগুলোকে সরল কাঠামোর মধ্যে আবদ্ধ করে। তবে, রুবি কমিউনিটি প্রায়ই RSpec এবং Minitest এর মত তৃতীয় পক্ষের লাইব্রেরিগুলোর দিকে ঝুঁকে থাকে কারণ এগুলোর ব্যক্তিগত প্রকাশভঙ্গি এবং নমনীয়তা বেশি।

### `Test::Unit` ব্যবহার করে:
```ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    result = 2 + 2
    assert_equal 4, result
  end
end
```
টার্মিনাল থেকে আপনার টেস্ট ফাইল রান করুন, এবং আপনি একটি আউটপুট পাবেন যা টেস্টগুলোর সফলতা বা ব্যর্থতা নির্দেশ করবে:
```
Loaded suite test_calculator
Started
.
Finished in 0.001288 seconds.
1 tests, 1 assertions, 0 failures, 0 errors, 0 pendings, 0 omissions, 0 notifications
100% passed
```

### RSpec ব্যবহার করে:
RSpec হল Ruby এর জন্য জনপ্রিয় একটি BDD (Behavior-Driven Development) ফ্রেমওয়ার্ক। `gem install rspec` দিয়ে জেম ইনস্টল করুন, তারপর আপনার প্রজেক্টে এটি ইনিশিয়ালাইজ করুন `rspec --init` দিয়ে।

```ruby
# calculator_spec.rb
require_relative '../calculator'

describe Calculator do
  it 'correctly adds two numbers' do
    expect(Calculator.add(2, 2)).to eq(4)
  end
end
```
`rspec` কমান্ড দিয়ে টেস্টগুলি রান করুন। উদাহরণ আউটপুট:
```
.

Finished in 0.002 seconds (files took 0.1 seconds to load)
1 example, 0 failures
```

### Minitest ব্যবহার করে:
Minitest TDD, BDD, মকিং, এবং বেঞ্চমার্কিং সাপোর্ট করে এমন একটি সম্পূর্ণ টেস্টিং ফ্যাসিলিটি প্রদান করে। `gem install minitest` দিয়ে এটি ইনস্টল করুন এবং নিম্নোক্ত উপায়ে ব্যবহার করুন:

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

মিনিটেস্টের জন্য সেট আপ করা `rake` টাস্ক বা সরাসরি আপনার টেস্ট ফাইল রান করুন। নমুনা আউটপুট:
```
Run options: --seed 33407

# Running:

.

Finished in 0.001027s, 974.5922 runs/s, 974.5922 assertions/s.
1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

এই লাইব্রেরিগুলি ব্যবহার করে আপনার রুবি প্রজেক্টে টেস্ট বাস্তবায়ন করে, আপনি সেরা অভ্যাসগুলিকে মেনে চলেন, যা আরো বিশ্বস্ত এবং রক্ষণাবেক্ষণযোগ্য কোড বেস গড়ে তুলে।
