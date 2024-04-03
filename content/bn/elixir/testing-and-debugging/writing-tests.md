---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:21.962644-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09AC\u09C7\u09A8\
  : Elixir \u098F\u09B0 \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u099F\u09C7\u09B8\
  \u09CD\u099F \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995\
  \ \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 ExUnit \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7, \u09AF\u09BE \u0985\u09A4\u09CD\u09AF\u09A8\u09CD\u09A4\
  \ \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u098F\u09AC\u0982 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u09B8\u09B9\u099C\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\u09B2: 1.\u2026"
lastmod: '2024-03-17T18:47:43.673772-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u098F\u09B0 \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u099F\u09C7\
  \u09B8\u09CD\u099F \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\
  \u0995 \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 ExUnit \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7, \u09AF\u09BE \u0985\u09A4\u09CD\u09AF\u09A8\u09CD\
  \u09A4 \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u098F\u09AC\u0982\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u09B8\u09B9\u099C\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u0989\
  \u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\u09B2."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

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
