---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:35.636685-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: \u09B8\u09C1\u0987\u09AB\u099F\
  \u09C7\u09B0 \u09AB\u09BE\u0989\u09A8\u09CD\u09A1\u09C7\u09B6\u09A8 \u09AB\u09CD\
  \u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995 \u09AB\u09BE\u0987\u09B2\
  \ \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\
  \u09A8\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF `FileManager` \u09A8\u09BE\u09AE\u09C7\
  \u09B0 \u098F\u0995\u099F\u09BF \u0995\u09CD\u09B2\u09BE\u09B8 \u09B8\u09B0\u09AC\
  \u09B0\u09BE\u09B9 \u0995\u09B0\u09C7\u0964 \u0986\u09AA\u09A8\u09BF `FileManager`\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0995\u09CB\u09A8\
  \u09CB\u2026"
lastmod: '2024-03-17T18:47:44.424447-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09C1\u0987\u09AB\u099F\u09C7\u09B0 \u09AB\u09BE\u0989\u09A8\u09CD\
  \u09A1\u09C7\u09B6\u09A8 \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\
  \u09CD\u0995 \u09AB\u09BE\u0987\u09B2 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\
  \ \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ `FileManager` \u09A8\u09BE\u09AE\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u0995\u09CD\
  \u09B2\u09BE\u09B8 \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\u09C7\u0964\
  \ \u0986\u09AA\u09A8\u09BF `FileManager` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 \u0995\u09CB\u09A8\u09CB \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\
  \u099F\u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u09AF\
  \u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u099F\u09BF \u0995\u09C0\u09AD\u09BE\
  \u09AC\u09C7 \u0995\u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7 \u09A4\u09BE\u09B0 \u098F\
  \u0995\u099F\u09BF \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09DF\
  \u09BE \u09B9\u09B2\u09CB."
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কীভাবে:
সুইফটের ফাউন্ডেশন ফ্রেমওয়ার্ক ফাইল সিস্টেম পরিচালনার জন্য `FileManager` নামের একটি ক্লাস সরবরাহ করে। আপনি `FileManager` ব্যবহার করে কোনো ডিরেক্টরির অস্তিত্ব যাচাই করতে পারেন। এখানে এটি কীভাবে করতে হবে তার একটি উদাহরণ দেওয়া হলো:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Directory exists")
} else {
    print("Directory does not exist")
}
```

তবে, এটি ফাইল এবং ডিরেক্টরিউভয়ের জন্য যাচাই করে। আপনি যদি নির্দিষ্টভাবে যাচাই করতে চান যে কোনো ডিরেক্টরির অস্তিত্ব রয়েছে, তবে আপনাকে `isDirectory` এ একটি বুলিয়ান মানের পয়েন্টার পাস করতে হবে:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("Directory exists")
} else {
    print("Directory does not exist")
}
```

### একটি থার্ড-পার্টি লাইব্রেরি ব্যবহার করা
বর্তমানে, সুইফটে ডিরেক্টরির অস্তিত্ব যাচাই করা সাধারণত `FileManager` ক্লাসের দৃঢ়তার কারনে থার্ড-পার্টি লাইব্রেরির প্রয়োজন হয় না। তবে, আরও জটিল ফাইল ম্যানিপুলেশন এবং যাচাইয়ের জন্য, জন সুন্ডেল কর্তৃক **Files** মতো লাইব্রেরি আরও সুইফট-বান্ধব API প্রদান করে।

আপনি কিভাবে এটি ব্যবহার করবেন তা এখানে দেওয়া হলো:

প্রথমে, সুইফট প্যাকেজ ম্যানেজারের মাধ্যমে আপনার প্রজেক্টে Files যোগ করুন।

তারপর, আপনি নিম্নলিখিত মতো একটি ডিরেক্টরির অস্তিত্ব যাচাই করতে পারেন:

```swift
import Files

do {
    _ = try Folder(path: "/path/to/your/directory")
    print("Directory exists")
} catch {
    print("Directory does not exist")
}
```

নোট: থার্ড-পার্টি লাইব্রেরি পরিবর্তনশীল হতে পারে, সবসময় ব্যবহার এবং সেরা প্র্যাক্টিসের জন্য সর্বশেষ ডকুমেন্টেশন দেখুন।
