---
title:                "ডিরেক্টরি আছে কিনা পরীক্ষা করা"
date:                  2024-03-17T17:45:35.636685-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?
ফাইল সিস্টেমে কোনো ডিরেক্টরির অস্তিত্ব যাচাই করা আপনার সুইফট অ্যাপ্লিকেশনের মধ্যে থেকে ফাইলের গঠন পরিচালনা করার জন্য অপরিহার্য। এই কাজটি ডেভেলপারদেরকে কোনো ডিরেক্টরিতে লিখন অথবা পড়ার চেষ্টা করার আগে তার উপস্থিতি যাচাই করতে সাহায্য করে, যার ফলে সম্ভাব্য রানটাইম ত্রুটি এড়ানো সম্ভব হয়।

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
