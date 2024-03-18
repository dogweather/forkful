---
title:                "টেস্ট লিখা"
date:                  2024-03-17T18:41:21.962644-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
Elixir ভাষায় টেস্ট লিখতে হলে আপনাকে অটোমেটেড স্ক্রিপ্টস তৈরি করতে হয়, যা আপনার কোডের আচরণ যাচাই করে। প্রোগ্রামাররা এটি করে থাকেন মান নির্ধারণ, পুনরাগমন এড়ানো, এবং কোড রিফ্যাক্টরিং সহজ করে তোলার জন্য, যা ডেভেলপমেন্ট প্রসেসকে আরো নির্ভরযোগ্য এবং কার্যকর করে তোলে।

## কিভাবে করবেন:
Elixir এর বিল্ট-ইন টেস্ট ফ্রেমওয়ার্ক হিসেবে ExUnit ব্যবহার করে, যা অত্যন্ত শক্তিশালী এবং ব্যবহার সহজ। এখানে একটি মৌলিক উদাহরণ দেওয়া হল:

1. আপনার Elixir প্রজেক্টের `test` ডিরেক্টরিতে একটি নতুন টেস্ট ফাইল তৈরি করুন। উদাহরণস্বরূপ, যদি আপনি `MathOperations` নামের একটি মডিউল টেস্ট করতে চান, তাহলে আপনার টেস্ট ফাইল হতে পারে `test/math_operations_test.exs`।

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # এটি একটি সহজ টেস্ট কেস যা দুই সংখ্যার যোগফল পরীক্ষা করে
  test "the addition of two numbers" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

আপনার টেস্টগুলি চালানোর জন্য, আপনার টার্মিনালে `mix test` কমান্ড ব্যবহার করুন। যদি `MathOperations.add/2` ফাংশনটি ঠিকভাবে দুইটি সংখ্যা যোগ করে, তাহলে আপনি দেখতে পাবেন নিম্নরূপ আউটপুটঃ

```
..

Finished in 0.03 seconds
1 test, 0 failures
```

বাইরের সার্ভিস বা API-এর সাথে জড়িত টেস্টগুলির জন্য, আসল সার্ভিসে আঘাত না করে `mox` এর মতো মক লাইব্রেরিগুলি ব্যবহার করতে চাইতে পারেনঃ

1. `mix.exs` এ আপনার নির্ভরতাগুলিতে `mox` যোগ করুনঃ

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # অন্যান্য নির্ভরতা...
  ]
end
```

2. আপনার টেস্ট হেল্পারে (`test/test_helper.exs`) একটি মক মডিউল ডিফাইন করুন:

```elixir
Mox.defmock(HTTPClientMock, for: HTTPClientBehaviour)
```

3. আপনার টেস্ট কেসে মকটি ব্যবহার করুন:

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # এটি Mox কে এই মকটি প্রত্যাশিতভাবে কল করা হয়েছে কিনা যাচাই করতে বলে
  setup :verify_on_exit!

  test "gets data from the API" do
    # মক রেসপন্স সেটআপ করুন
    expect(HTTPClientMock, :get, fn _url -> {:ok, "Mocked response"} end)
    
    assert SomeAPIClient.get_data() == "Mocked response"
  end
end
```

`mix test` চালানোর সময়, এই সেটআপটি আপনাকে বাইরের আসল নির্ভরতাগুলি থেকে আপনার ইউনিট টেস্টগুলি পৃথক করতে দেয়, যাতে আপনি নিজের কোডের আচরণের উপর মনোনিবেশ করতে পারেন। এই প্যাটার্নটি নিশ্চিত করে যে আপনার টেস্টগুলি দ্রুত এবং বাইরের সার্ভিসের অবস্থা বা ইন্টারনেট সংযোগ থাকার স্বত্ত্বেও নির্ভরযোগ্য থাকে।
