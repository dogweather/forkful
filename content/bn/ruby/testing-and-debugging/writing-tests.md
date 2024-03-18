---
title:                "টেস্ট লিখা"
date:                  2024-03-17T18:41:55.887568-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
রুবিতে টেস্টিং মানে হল বিভিন্ন পরিস্থিতিতে আপনার কোড প্রত্যাশিতভাবে আচরণ করছে কিনা তা নিরীক্ষণ করা। প্রোগ্রামাররা নির্ভুলতা নিশ্চিত করতে, রিগ্রেশন প্রতিরোধ করতে এবং রিফ্যাক্টরিং সহজ করতে টেস্ট লেখেন, রোবাস্ট এবং রক্ষণাবেক্ষণযোগ্য অ্যাপ্লিকেশনের লক্ষ্য নিয়ে।

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
