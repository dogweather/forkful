---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:45.995942-06:00
description: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\
  \u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0987\u09A8\
  \u09CD\u099F\u09BE\u09B0\u09A8\u09C7\u099F\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 \u098F\u09B0 \u09B8\u09BE\u09AE\u0997\u09CD\u09B0\u09C0 \u09AA\u09C7\
  \u09A4\u09C7\u2014\u09AE\u09C2\u09B2\u09A4, \u0986\u09AA\u09A8\u09BE\u09B0 \u09AC\
  \u09CD\u09B0\u09BE\u0989\u099C\u09BE\u09B0 \u09AF\u09BE \u0995\u09B0\u09C7\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09A1\u09C7\u099F\u09BE \u098F\u0995\u09CD\u09B8\u099F\u09CD\u09B0\u09CD\u09AF\
  \u09BE\u0995\u09B6\u09A8, \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982, \u0985\u09A5\
  \u09AC\u09BE \u0997\u09CD\u09B0\u09BE\u09AB\u09BF\u0995\u09CD\u09AF\u09BE\u09B2\u2026"
lastmod: '2024-03-17T18:47:43.667684-06:00'
model: gpt-4-0125-preview
summary: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\u09A8\
  \u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0987\u09A8\u09CD\
  \u099F\u09BE\u09B0\u09A8\u09C7\u099F\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\
  \u09C7 \u098F\u09B0 \u09B8\u09BE\u09AE\u0997\u09CD\u09B0\u09C0 \u09AA\u09C7\u09A4\
  \u09C7\u2014\u09AE\u09C2\u09B2\u09A4, \u0986\u09AA\u09A8\u09BE\u09B0 \u09AC\u09CD\
  \u09B0\u09BE\u0989\u099C\u09BE\u09B0 \u09AF\u09BE \u0995\u09B0\u09C7\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\
  \u099F\u09BE \u098F\u0995\u09CD\u09B8\u099F\u09CD\u09B0\u09CD\u09AF\u09BE\u0995\u09B6\
  \u09A8, \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982, \u0985\u09A5\u09AC\u09BE \u0997\
  \u09CD\u09B0\u09BE\u09AB\u09BF\u0995\u09CD\u09AF\u09BE\u09B2 \u0987\u0989\u099C\u09BE\
  \u09B0 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AB\u09C7\u09B8 \u099B\u09BE\u09A1\
  \u09BC\u09BE\u0987 \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\
  \u09BF\u09B8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09C7\u0995\u09CD\u099F \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09C7\u0964."
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কিভাবে:
এলিক্সির, এর শক্তিশালী HTTP ক্লায়েন্ট লাইব্রেরি সহ, এই কাজটি সহজ করে তোলে। এখানে `HTTPoison` দিয়ে কিভাবে করা যায়:

```elixir
# প্রথমে, আপনার mix.exs নির্ভরতায় HTTPoison যোগ করুন:
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# mix deps.get চালান নতুন নির্ভরতা পেতে

# এখন, চলুন একটি ওয়েব পেজ ডাউনলোড করা যাক:
defmodule PageDownloader do
  def download(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:error, "Received status code: #{status_code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end
end

# উদাহরণ ব্যবহার:
{:ok, contents} = PageDownloader.download("http://example.com")
```

ক্র্যাশ এড়াতে সম্ভাব্য ত্রুটিগুলি সংবলিত করা নিশ্চিত করুন!

## গভীর ডাইভ
এলিক্সিরের ওয়েব ইন্টারেকশন পদ্ধতি এরল্যাং-এর দৃঢ় নেটওয়ার্কিং ক্ষমতার দ্বারা চালিত। `HTTPoison` হল `hackney`-এর উপর নির্মিত একটি জনপ্রিয় লাইব্রেরি, তবে এটি একমাত্র প্লেয়ার নয়। মিডওয়্যার সাপোর্ট সহ একটি আরও মডুলার পদ্ধতির প্রস্তাব দেওয়া, `Tesla` রয়েছে।

ঐতিহাসিকভাবে, ওয়েব সামগ্রী ডাউনলোড করা আরও ম্যানুয়াল ছিল, সকেট ওপর দিয়ে HTTP অনুরোধ তৈরি করে। এলিক্সির লাইব্রেরিগুলি এই বিবরণগুলি এবস্ট্রাক্ট করে, আপনাকে আপনার অ্যাপ্লিকেশন লজিকে মনোনিবেশ করতে দেয় এর পরিবর্তে।

ওয়েব পেজ ডাউনলোড করার সময়, আপনি অ্যাসিঙ্ক্রোনাস অপারেশন এবং বিভিন্ন HTTP প্রটোকলের সাথে মোকাবিলা করেন, যা এলিক্সির এর পারস্পরিক মডেল এবং দোষসহিষ্ণু ডিজাইনের কারণে সুন্দরভাবে সামলায়। তাছাড়া, টেক্সট এবং বাইনারি ডেটা হ্যান্ডলিং ক্রিটিক্যাল—অবশ্যই ইনকোডিংয়ের প্রতি মনোযোগী হন এবং ওয়েব সামগ্রীতে বাইনারি ডেটার সম্ভাবনা বিবেচনা করুন।

## আরও দেখুন
- [`HTTPoison` ডকুমেন্টেশন](https://hexdocs.pm/httpoison)
- [হেক্সে `Tesla` লাইব্রেরি](https://hex.pm/packages/tesla)
- [এলিক্সির স্কুলের OTP কনকারেন্সি গাইড](https://elixirschool.com/en/lessons/advanced/otp-concurrency/)
- [এরল্যাং-এর `hackney` লাইব্রেরি](https://github.com/benoitc/hackney)
