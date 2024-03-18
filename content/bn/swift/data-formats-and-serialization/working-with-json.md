---
title:                "JSON এর সাথে কাজ করা"
date:                  2024-03-17T18:29:52.938272-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

Swift-এ JSON নিয়ে কাজ করা মানে ডেটা আদান-প্রদানের জন্য একটি হালকা ডেটা ফরম্যাটের সাথে ডিল করা। প্রোগ্রামাররা একটি সার্ভার এবং একটি ওয়েব অ্যাপ্লিকেশনের মধ্যে ডেটা প্রেরণের জন্য JSON ব্যবহার করে থাকেন কারণ এটি মানুষ এবং মেশিনের জন্য পড়া এবং পার্স করা সহজ।

## কিভাবে:

Swift `Codable` প্রটোকলের মাধ্যমে JSON পার্সিং সরল করে তোলে। এখানে JSON থেকে Swift অবজেক্টে ডিকোড করার উপায় দেওয়া হল:

```Swift
import Foundation

// Codable অনুযায়ী একটি মডেল ডিফাইন করা
struct User: Codable {
    var name: String
    var age: Int
}

// JSON string
let jsonString = """
{
    "name": "John Doe",
    "age": 30
}
"""

// JSON string থেকে Data-এ কনভার্ট করা
if let jsonData = jsonString.data(using: .utf8) {
    // JSON data থেকে User অবজেক্টে ডিকোড করা
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("Name: \(user.name), Age: \(user.age)")
    } catch {
        print("Error decoding JSON: \(error)")
    }
}
```

নমুনা আউটপুট:
```
Name: John Doe, Age: 30
```

## গভীর ডাইভ

JSON (JavaScript Object Notation) ২০০০ সালের প্রারম্ভে ডগলাস ক্রকফোর্ড এর নির্দিষ্ট করার পর থেকে বিস্তৃতভাবে গ্রহণযোগ্য হয়ে উঠেছে। এর সাধারণ সিনট্যাক্স এবং ভালো পারফরমেন্সের কারণে এটি অনেক ক্ষেত্রে XML-কে প্রতিস্থাপন করেছে। Swift-এর `Codable` হলো JSON এর জন্য প্রাথমিক পছন্দ, তবে `JSONSerialization` মত বিকল্পগুলি অ-কোডেবল কমপ্লায়েন্ট টাইপগুলি নিয়ে কাজ করার জন্য বিদ্যমান রয়েছে। আড়ালে, `Codable` নিম্ন-স্তরের পার্সিংকে অ্যাবস্ট্র্যাক্ট করে এবং সিরিয়ালাইজেশন/ডিসিরিয়ালাইজেশনকে সহজ করে তোলে।

## দেখুন

- অফিসিয়াল Swift ব্লগে JSON এবং Swift সম্পর্কে আরও জানুন: [Swift.org](https://swift.org/blog/)
- `Codable` ডকুমেন্টেশন দেখুন: [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- জটিল JSON স্ট্রাকচারের ক্ষেত্রে, SwiftyJSON মত তৃতীয়-পক্ষের লাইব্রেরিগুলি বিবেচনা করুন যা পাওয়া যায় [GitHub](https://github.com/SwiftyJSON/SwiftyJSON)-এ।
