---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:32.447686-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Fish Shell \u09AB\u09BE\u0987\u09B2\
  \u09C7\u09B0 \u09A7\u09B0\u09A8 \u098F\u09AC\u0982 \u09AC\u09C8\u09B6\u09BF\u09B7\
  \u09CD\u099F\u09CD\u09AF \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF `test` \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7, \u09AF\u09BE \u09A8\u09BF\u09B0\
  \u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u098F\u0995\u099F\u09BF \u09B2\u0995\u09CD\
  \u09B7\u09CD\u09AF\u09AE\u09BE\u09A4\u09CD\u09B0\u09BE \u09A1\u09BF\u09B0\u09C7\u0995\
  \u09CD\u099F\u09B0\u09BF \u0995\u09BF\u09A8\u09BE \u09A4\u09BE \u09B8\u09B9 \u099C\
  \u09BE\u09A8\u09BE \u09AF\u09BE\u09AF\u09BC\u0964\u2026"
lastmod: '2024-03-17T18:47:44.512897-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u09A7\u09B0\u09A8 \u098F\
  \u09AC\u0982 \u09AC\u09C8\u09B6\u09BF\u09B7\u09CD\u099F\u09CD\u09AF \u09AF\u09BE\
  \u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF `test` \u0995\
  \u09AE\u09BE\u09A8\u09CD\u09A1 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7, \u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F\
  \ \u098F\u0995\u099F\u09BF \u09B2\u0995\u09CD\u09B7\u09CD\u09AF\u09AE\u09BE\u09A4\
  \u09CD\u09B0\u09BE \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0995\
  \u09BF\u09A8\u09BE \u09A4\u09BE \u09B8\u09B9 \u099C\u09BE\u09A8\u09BE \u09AF\u09BE\
  \u09AF\u09BC\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A1\
  \u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\
  \u09A4\u09CD\u09AC \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0 \u09AA\
  \u09CD\u09B0\u09BE\u09A5\u09AE\u09BF\u0995 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\
  \u09CD\u09A8 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

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
