---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:34.506910-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u09B0 \u09AE\u09C2\u09B2\
  \u09C7, Bash \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09B6\u09B0\u09CD\u09A4\u09AE\
  \u09C2\u09B2\u0995 \u09AC\u09BF\u09AC\u09C3\u09A4\u09BF \u098F\u09AC\u0982 `-d`\
  \ \u0985\u09AA\u09BE\u09B0\u09C7\u099F\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09A1\u09BF\u09B0\u09C7\u0995\
  \u09CD\u099F\u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC\
  \ \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09A4\u09C7 \u09A6\u09C7\
  \u09AF\u09BC\u0964 \u09A8\u09C0\u099A\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09B0\
  \u09B2 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\
  \ \u09B9\u09B2 \u09AF\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.242383-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u09B0 \u09AE\u09C2\u09B2\u09C7, Bash \u0986\u09AA\u09A8\u09BE\u0995\
  \u09C7 \u09B6\u09B0\u09CD\u09A4\u09AE\u09C2\u09B2\u0995 \u09AC\u09BF\u09AC\u09C3\
  \u09A4\u09BF \u098F\u09AC\u0982 `-d` \u0985\u09AA\u09BE\u09B0\u09C7\u099F\u09B0\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\
  \u09BF \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u0985\u09B8\
  \u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE\
  \ \u0995\u09B0\u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u09A8\u09C0\u099A\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09B0\u09B2 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2 \u09AF\u09BE \u09A6\u09C7\u0996\
  \u09BE\u09AF\u09BC \u098F\u0987 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE\u099F\
  \u09BF \u0995\u09C0\u09AD\u09BE\u09AC\u09C7 \u09B8\u09AE\u09CD\u09AA\u09BE\u09A6\
  \u09A8 \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC\u0964."
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কিভাবে:
এর মূলে, Bash আপনাকে শর্তমূলক বিবৃতি এবং `-d` অপারেটর ব্যবহার করে একটি ডিরেক্টরির অস্তিত্ব পরীক্ষা করতে দেয়। নীচে একটি সরল উদাহরণ দেওয়া হল যা দেখায় এই পরীক্ষাটি কীভাবে সম্পাদন করা যায়।

```bash
if [ -d "/path/to/directory" ]; then
    echo "ডিরেক্টরি অস্তিত্ব আছে।"
else
    echo "ডিরেক্টরি অস্তিত্ব নেই।"
fi
```

নমুনা আউটপুট (যদি ডিরেক্টরি অস্তিত্ব থাকে):
```
ডিরেক্টরি অস্তিত্ব আছে।
```

নমুনা আউটপুট (যদি ডিরেক্টরি অস্তিত্ব না থাকে):
```
ডিরেক্টরি অস্তিত্ব নেই।
```

আরও জটিল স্ক্রিপ্টের ক্ষেত্রে, ডিরেক্টরি না থাকলে এটি তৈরি করার মতো অন্যান্য অপারেশনের সাথে পরীক্ষাটি মেলানো সাধারণ:

```bash
DIR="/path/to/directory"
if [ -d "$DIR" ]; then
    echo "$DIR অস্তিত্ব আছে।"
else
    echo "$DIR অস্তিত্ব নেই। এখন তৈরি করা হচ্ছে..."
    mkdir -p "$DIR"
    echo "$DIR তৈরি হয়েছে।"
fi
```

নমুনা আউটপুট (যদি ডিরেক্টরি অস্তিত্ব না থাকে এবং তারপরে তৈরি করা হয়):
```
/path/to/directory অস্তিত্ব নেই। এখন তৈরি করা হচ্ছে...
/path/to/directory তৈরি হয়েছে।
```

যদিও Bash নিজেই এই ধরনের পরীক্ষাগুলির জন্য বলিষ্ঠ সরঞ্জাম প্রদান করে, এই কাজের জন্য কোনো জনপ্রিয় তৃতীয়-পক্ষের লাইব্রেরি নেই, কারণ দেশীয় Bash কমান্ডগুলি ডিরেক্টরির উপস্থিতি যাচাই করতে সম্পূর্ণ সক্ষম এবং দক্ষ।
