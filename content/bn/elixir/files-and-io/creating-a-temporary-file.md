---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:04.391517-06:00
description: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u098F\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0987\u09B2\
  \ \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AF\u09BE \u0986\u09AA\u09A8\u09BF\
  \ \u09B8\u0982\u0995\u09CD\u09B7\u09BF\u09AA\u09CD\u09A4 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0\u09C7\u09B0 \u09AA\u09B0\u09C7 \u09AB\u09C7\u09B2\u09C7 \u09A6\
  \u09BF\u09AC\u09C7\u09A8\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\
  \u09BC\u09C0 \u09B8\u099E\u09CD\u099A\u09AF\u09BC\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09AC\u09BE \u09AF\u0996\u09A8 \u09A4\u09BE\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.691745-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u098F\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0987\u09B2\
  \ \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AF\u09BE \u0986\u09AA\u09A8\u09BF\
  \ \u09B8\u0982\u0995\u09CD\u09B7\u09BF\u09AA\u09CD\u09A4 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0\u09C7\u09B0 \u09AA\u09B0\u09C7 \u09AB\u09C7\u09B2\u09C7 \u09A6\
  \u09BF\u09AC\u09C7\u09A8\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\
  \u09BC\u09C0 \u09B8\u099E\u09CD\u099A\u09AF\u09BC\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09AC\u09BE \u09AF\u0996\u09A8 \u09A4\u09BE\u09B0\u09BE\u2026"
title: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
একটি অস্থায়ী ফাইল তৈরি করা মানে এমন একটি ফাইল তৈরি করা যা আপনি সংক্ষিপ্ত ব্যবহারের পরে ফেলে দিবেন। প্রোগ্রামাররা এটি অস্থায়ী সঞ্চয়ের জন্য বা যখন তারা অতি-সংক্ষিপ্ত জীবনকাল সহ ডেটা দিয়ে একটি হার্ড ড্রাইভ বাধাগ্রস্ত করতে চান না, তখন এটি করে থাকে।

## কিভাবে:
Elixir-এ, আপনি `System.tmp_dir/1` ফাংশন এবং `File` মডিউল দিয়ে একটি অস্থায়ী ফাইল তৈরি এবং ব্যবহার করতে পারেন। এটা একটি দ্রুত উদাহরণ:

```elixir
# আসুন, আমরা আমাদের হাত গুটিয়ে এটি শুরু করি!

# অস্থায়ী ডিরেক্টরি খুঁজুন
temp_dir = System.tmp_dir!()

# একটি অস্থায়ী ফাইল পথ তৈরি করুন
temp_file_path = Path.join(temp_dir, "my_temp_file.txt")

# আসুন কিছু অস্থায়ী লিখি
File.write!(temp_file_path, "Hello, temporary world!")

# এটি পড়ুন, শুধু নিশ্চিত হতে যে সব ঠিক আছে
IO.puts(File.read!(temp_file_path))

# নিজেদের পরিষ্কার করুন এবং অস্থায়ী ফাইলটি মুছে ফেলুন
File.rm!(temp_file_path)
```

নমুনা আউটপুট:
```
Hello, temporary world!
```

## গভীর ডুব
অস্থায়ী ফাইলগুলি কেবল Elixir-এ অনন্য নয়। তারা প্রোগ্রামিং ভাষাগুলি জুড়ে একটি প্রধান উপাদান কারণ তারা এমন ডেটা পরিচালনা করার জন্য উত্তম যা শুধু একটি প্রোগ্রামের নির্বাহ সময়ের সময় গুরুত্বপূর্ণ হয়। স্টোরেজ সস্তা হয়ে যাওয়ার আগে, ডিস্ক স্পেস সাশ্রয় করা জরুরি ছিল—অস্থায়ী ফাইলগুলি এতে সাহায্য করেছিল। আজকে, তারা সম্পদ পরিচালনা এবং নিরাপত্তা জন্য সুবিধাজনক: কম স্থায়ী ডেটা মানে কম চিহ্ন রাখা পিছনে।

বিকল্প হিসেবে, Elixir-এ, আপনি নিজের অস্থায়ী ফাইল লজিক তৈরি করতে পারেন অথবা সরাসরি Erlang ফাংশনগুলি ব্যবহার করতে পারেন (যেমন, `:erlang.mktemp/0`)। এবং বিস্তারিত, যখন আপনি একটি অস্থায়ী ফাইল তৈরি করেন, বিস্তারিত—যেমন নামকরণ—আপনার OS দ্বারা পরিচালিত হয়, Elixir নিজেই নয়। Elixir শুধুমাত্র OS-কে জিজ্ঞাসা করে ফাইলটিকে অস্থায়ীভাবে কোথায় রাখতে, এবং OS সাড়া দেয়।

## আরও দেখুন
Elixir ফাইল ম্যানিপুলেশনের জন্য আরও:
- Elixir-এর `File` মডিউল: https://hexdocs.pm/elixir/File.html
- `System.tmp_dir/1`-এর জন্য অফিসিয়াল ডকুমেন্টেশন: https://hexdocs.pm/elixir/System.html#tmp_dir/1

Erlang-এর ফাইল ম্যানেজমেন্ট ক্ষমতা অন্বেষণ:
- Erlang-এর `file` মডিউল: http://erlang.org/doc/man/file.html
