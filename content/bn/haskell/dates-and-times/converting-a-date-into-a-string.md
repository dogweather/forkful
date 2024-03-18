---
title:                "তারিখকে স্ট্রিং এ রূপান্তর করা"
date:                  2024-03-17T17:46:58.335638-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
একটি তারিখকে টেক্সটে রূপান্তর করা মানে একটি তারিখ অবজেক্টকে পাঠ্যযোগ্য টেক্সটে পরিণত করা। প্রোগ্রামাররা এটা করে থাকেন ব্যবহারকারীদের কাছে তারিখ দেখানো অথবা সংরক্ষণ অথবা প্রেরণের জন্য এগুলোকে বিন্যাস করার লক্ষ্যে।

## কিভাবে:
Haskell-এ, এই কাজের জন্য আপনি `Data.Time.Format` মডিউল থেকে `formatTime` ফাংশন ব্যবহার করেন। চলুন কিছু কোডের মধ্য দিয়ে সরাসরি ডুব দেই:

```haskell
import Data.Time
import Data.Time.Format (formatTime, defaultTimeLocale)

main :: IO ()
main = do
    -- বর্তমান সময় আহরণ করুন
    currentTime <- getCurrentTime
    let currentZone = utc
        -- UTC সময়কে একটি স্থানীয় সময় অবজেক্টে রূপান্তর করুন
        localTime = utcToLocalTime currentZone currentTime
        -- তারিখটিকে "YYYY-MM-DD" আকারে বিন্যাস করুন
        dateString = formatTime defaultTimeLocale "%F" localTime
    putStrLn dateString
```

এবং বর্তমান তারিখের উপর নির্ভর করে, আপনি যা আউটপুট হিসেবে দেখতে পাবেন:

```
2023-04-01
```

## গভীর ডুব
প্রোগ্রামিং-এর প্রারম্ভিক দিনগুলিতে ফিরে গেলে, তারিখগুলিকে স্ট্রিং-এ রূপান্তর করা সবসময়ই বাস্তব ব্যবহার্যতার একটি বিষয় ছিল। Haskell-এ, আমরা আমাদের তারিখ এবং সময় পরিচালনার জন্য `Data.Time` লাইব্রেরিকে ধন্যবাদ জানাই, যা `old-time` এর মতো পুরানো লাইব্রেরিগুলির ওপর উন্নতি এবং কার্যকারিতার প্রেরণা থেকে অনুপ্রাণিত হয়েছে।

`formatTime`-এর বিকল্প হিসেবে, আপনি সরাসরি একটি তারিখকে স্ট্রিং-এ রূপান্তর করতে `show` ব্যবহার করতে পারেন, কিন্তু এটি আপনাকে কাস্টম বিন্যাসের বিকল্প দেবে না। `formatTime` ফাংশন সমৃদ্ধ, C-এর `strftime` ফাংশন প্যাটার্নগুলির সঙ্গে মিল রেখে নানা ধরনের বিন্যাস সমর্থন করে। এটি নমনীয় এবং লোকাল সচেতন, `defaultTimeLocale` অথবা অন্যান্য লোকাল ব্যবহার করে তারিখগুলিকে সাংস্কৃতিক রীতিনীতি অনুযায়ী বিন্যাস করে।

বাস্তবায়নের দিক থেকে, `Data.Time.Format` ফাংশনগুলি শুদ্ধ, যার মানে এগুলি কোনো পার্শ্ব প্রভাবের উপর নির্ভর করে না অথবা ঘটায় না। এটি Haskell-এর ফাংশনাল প্রোগ্রামিং আদর্শের সঙ্গে মিলে যায়, যেখানে লক্ষ্য হল ফাংশনগুলি প্রেডিক্টেবল হওয়া এবং তাদের ফলাফল কেবলমাত্র তাদের ইনপুট দ্বারা নির্ধারিত হওয়া।

## আরও দেখুন
Haskell-এ তারিখ এবং সময় নিয়ে বিস্তারিত কাজের জন্য, নিম্নলিখিত দেখুন:

- `Data.Time` মডিউল ডকুমেন্টেশন: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- `strftime` বিন্যাস স্ট্রিংস সম্পর্কে বিস্তারিত, যা `formatTime` অনুকরণ করে: [http://man7.org/linux/man-pages/man3/strftime.3.html](http://man7.org/linux/man-pages/man3/strftime.3.html)
- আইও এবং শুদ্ধতা নিয়ে Haskell-এর প্রবণতা: [https://www.haskell.org/tutorial/io.html](https://www.haskell.org/tutorial/io.html)
