---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:37.634424-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09A1\u09BE\u09B0\u09CD\u099F\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE \u09A6\
  \u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u0995\u09AF\u09BC\u09C7\u0995\u099F\u09BF\
  \ \u09B8\u09B0\u09B2 \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\u09BE\
  \u09A8 \u0995\u09B0\u09C7\u0964 \u09A8\u09BF\u09AE\u09CD\u09A8\u09C7 \u09B8\u09B0\
  \u09CD\u09AC\u09BE\u09A7\u09BF\u0995 \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u09AE\
  \u09C7\u09A5\u09A1\u0997\u09C1\u09B2\u09BF \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\
  \u09B2."
lastmod: '2024-04-05T21:53:51.810706-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BE\u09B0\u09CD\u099F \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u099C\u09CB\u09A1\u09BC\u09BE \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u0995\
  \u09AF\u09BC\u09C7\u0995\u099F\u09BF \u09B8\u09B0\u09B2 \u0989\u09AA\u09BE\u09AF\
  \u09BC \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u09A8\u09BF\
  \u09AE\u09CD\u09A8\u09C7 \u09B8\u09B0\u09CD\u09AC\u09BE\u09A7\u09BF\u0995 \u09B8\
  \u09BE\u09A7\u09BE\u09B0\u09A3 \u09AE\u09C7\u09A5\u09A1\u0997\u09C1\u09B2\u09BF\
  \ \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\u09B2."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
weight: 3
---

## কিভাবে:
ডার্ট স্ট্রিং জোড়া দেওয়ার কয়েকটি সরল উপায় প্রদান করে। নিম্নে সর্বাধিক সাধারণ মেথডগুলি দেওয়া হল:

### `+` অপারেটর ব্যবহার করে
`+` অপারেটর স্ট্রিংগুলি জোড়া দেওয়ার সবচেয়ে সহজবোধ্য উপায়।
```dart
String greeting = 'হ্যালো, ' + 'ওয়ার্ল্ড!';
print(greeting); // আউটপুট: হ্যালো, ওয়ার্ল্ড!
```

### `concat()` মেথড ব্যবহার করে
ডার্টের অন্যান্য ভাষার মতো `concat()` মেথড নেই, তবে একই কাজ `+` বা নিম্নলিখিত মেথড ব্যবহার করে সম্পন্ন করা যায়।

### স্ট্রিং ইন্টারপোলেশন ব্যবহার করে
স্ট্রিং ইন্টারপোলেশন একটি স্ট্রিং-এ সরাসরি ভেরিয়েবল এম্বেড করে। এটি স্ট্রিং এবং এক্সপ্রেশনগুলি জোড়া দেওয়ার জন্য দক্ষ।
```dart
String user = 'জেন';
String message = 'স্বাগতম, $user!';
print(message); // আউটপুট: স্বাগতম, জেন!
```

### `join()` মেথড ব্যবহার করে
`join()` মেথড উপযোগী যখন আপনার কাছে একটি স্ট্রিং তালিকা থাকে যা আপনি জোড়া দিতে চান।
```dart
var words = ['হ্যালো', 'ডার্ট থেকে'];
String sentence = words.join(' '); // একটি স্পেস সেপারেটর দিয়ে জয়েন।
print(sentence); // আউটপুট: হ্যালো ডার্ট থেকে
```

### `StringBuffer` ব্যবহার করে
একাধিক জোড়া দেওয়ার জন্য `StringBuffer` দক্ষ, বিশেষ করে লুপের মধ্যে।
```dart
var words = ['ডার্ট', 'মজা', 'আছে'];
StringBuffer buffer = StringBuffer();
for (String word in words) {
  buffer.write(word); // প্রত্যেক শব্দের সাথে বাফারটি যুক্ত করুন।
  buffer.write(' '); // ঐচ্ছিকভাবে একটি স্পেস যোগ করুন।
}
String sentence = buffer.toString().trim(); // স্ট্রিং-এ রূপান্তর করুন এবং সর্বশেষ স্পেস সরান।
print(sentence); // আউটপুট: ডার্ট মজা আছে
```

### থার্ড-পার্টি লাইব্রেরি
যদিও ডার্টের মানক লাইব্রেরি সাধারণত স্ট্রিং জোড়া দেওয়ার কাজের জন্য যথেষ্ট, তথাপি থার্ড-পার্টি লাইব্রেরি যেমন `quiver` ডার্টের নির্মিত কার্যকারিতাকে পূরক করতে পারে। উদাহরণস্বরূপ, উন্নত পরিস্থিতির জন্য `quiver`-এর `concat()` অথবা `merge()` ফাংশন পরীক্ষা করা যেতে পারে। তবে, যদি ডার্টের রবাস্ট নির্মিত বিকল্পগুলি আপনার নির্দিষ্ট প্রয়োজন পূরণ না করে, তবে তার পরিবর্তে থার্ড-পার্টি লাইব্রেরিগুলিতে না যেতেই ভালো।
