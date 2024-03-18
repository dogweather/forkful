---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:32.447686-06:00
description: "Fish Shell \u098F \u098F\u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\
  \u09CD\u099F\u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC\
  \ \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09A1\u09BF\
  \u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BE\u0995\
  \u099A\u09BE\u09B0\u09C7\u09B0 \u0989\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF\
  \ \u09AC\u09BE \u0985\u09A8\u09C1\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u09AD\
  \u09BF\u09A4\u09CD\u09A4\u09BF\u0995 \u09B8\u09BF\u09A6\u09CD\u09A7\u09BE\u09A8\u09CD\
  \u09A4 \u0997\u09CD\u09B0\u09B9\u09A3 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7, \u09AF\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.512897-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell \u098F \u098F\u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\
  \u09CD\u099F\u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC\
  \ \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09A1\u09BF\
  \u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BE\u0995\
  \u099A\u09BE\u09B0\u09C7\u09B0 \u0989\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF\
  \ \u09AC\u09BE \u0985\u09A8\u09C1\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u09AD\
  \u09BF\u09A4\u09CD\u09A4\u09BF\u0995 \u09B8\u09BF\u09A6\u09CD\u09A7\u09BE\u09A8\u09CD\
  \u09A4 \u0997\u09CD\u09B0\u09B9\u09A3 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7, \u09AF\u09BE\u2026"
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
Fish Shell এ একটি ডিরেক্টরির অস্তিত্ব যাচাই করা মানে স্ক্রিপ্টগুলি ডিরেক্টরি স্ট্রাকচারের উপস্থিতি বা অনুপস্থিতি ভিত্তিক সিদ্ধান্ত গ্রহণ করতে পারে, যা শর্তাধীন ফাইল অপারেশন, লগিং অথবা পরিবেশ সেটআপের মত কাজকে সম্ভব করে তোলে। ফাইল সিস্টেমের সাথে পুনরাবৃত্তিধর্মী ভাবে মিথস্ক্রিয়া করে এমন স্ক্রিপ্ট লিখতে এই কৌশল খুবই গুরুত্বপূর্ণ।

## কিভাবে:
Fish Shell ফাইলের ধরন এবং বৈশিষ্ট্য যাচাই করার জন্য `test` কমান্ড ব্যবহার করে, যা নির্দিষ্ট একটি লক্ষ্যমাত্রা ডিরেক্টরি কিনা তা সহ জানা যায়। এখানে একটি ডিরেক্টরির অস্তিত্ব যাচাই করার প্রাথমিক প্যাটার্ন দেওয়া হল:

```fish
if test -d /path/to/dir
    echo "Directory exists"
else
    echo "Directory does not exist"
end
```
নমুনা আউটপুট:
```
Directory exists
```

আরও স্ট্রিমলাইনড ফাইল এবং ডিরেক্টরি অপারেশনের জন্য, কেউ `fd` এর মত বাহ্যিক টুলের দিকে ঝুঁকতে পারেন, যদিও এটি শুধুমাত্র ফাইল এবং ডিরেক্টরিগুলি খুঁজে পাওয়ার জন্য বেশি ব্যবহৃত হয় না। তবে, এটিকে Fish স্ক্রিপ্টিং এর সাথে মিলিয়ে ব্যবহার করলে দারুণ ফলাফল মিলতে পারে:

```fish
set dir "/path/to/search"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "Directory exists"
else
    echo "Directory does not exist"
end
```

এই `fd` নমুনাটি নির্দিষ্ট গভীরতা পর্যন্ত ডিরেক্টরি খুঁজে বের করে, এবং `grep` মিলান চেক করে, যা জটিল চেকের জন্য বহুমুখী হয়। তবে, শুধুমাত্র অস্তিত্ব যাচাইয়ের সরাসরি উদ্দেশ্যের জন্য, Fish এর নিজস্ব `test` ব্যবহার করা দক্ষ এবং সরাসরি।
