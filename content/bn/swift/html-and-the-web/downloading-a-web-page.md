---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:38.093730-06:00
description: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\
  \u099C \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u0993\u09AF\u09BC\u09C7\u09AC \u09A5\u09C7\u0995\u09C7 \u09A4\u09A5\
  \u09CD\u09AF \u09A8\u09C7\u0993\u09AF\u09BC\u09BE \u098F\u09AC\u0982 \u09A4\u09BE\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u0985\u09CD\u09AF\u09BE\u09AA\u09C7 \u0986\u09A8\
  \u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09AB\u09C7\u099A \u0995\
  \u09B0\u09A4\u09C7, \u0985\u09A8\u09B2\u09BE\u0987\u09A8 \u09B8\u09BE\u09B0\u09CD\
  \u09AD\u09BF\u09B8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0987\u09A8\u09CD\u099F\
  \u09BE\u09B0\u098F\u0995\u09CD\u099F \u0995\u09B0\u09A4\u09C7 \u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.408165-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u0993\u09AF\u09BC\u09C7\u09AC \u09A5\u09C7\u0995\u09C7 \u09A4\u09A5\u09CD\
  \u09AF \u09A8\u09C7\u0993\u09AF\u09BC\u09BE \u098F\u09AC\u0982 \u09A4\u09BE \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u0985\u09CD\u09AF\u09BE\u09AA\u09C7 \u0986\u09A8\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09AB\u09C7\u099A \u0995\u09B0\
  \u09A4\u09C7, \u0985\u09A8\u09B2\u09BE\u0987\u09A8 \u09B8\u09BE\u09B0\u09CD\u09AD\
  \u09BF\u09B8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u098F\u0995\u09CD\u099F \u0995\u09B0\u09A4\u09C7 \u09AC\u09BE \u09A1\u09BE\
  \u099F\u09BE \u09B8\u09CD\u0995\u09CD\u09B0\u09CD\u09AF\u09BE\u09AA \u0995\u09B0\
  \u09A4\u09C7 \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8\
  \u0964."
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কি এবং কেন?
একটি ওয়েব পেজ ডাউনলোড করা মানে ওয়েব থেকে তথ্য নেওয়া এবং তা আপনার অ্যাপে আনা। প্রোগ্রামাররা কন্টেন্ট ফেচ করতে, অনলাইন সার্ভিসের সাথে ইন্টারএক্ট করতে বা ডাটা স্ক্র্যাপ করতে এটি করে থাকেন।

## কিভাবে:
`URLSession` ব্যবহার করে এই কাজটি করা যাক। Swift এটি সরাসরি উপস্থাপন করে।

```Swift
import Foundation

let url = URL(string: "https://www.example.com")!
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("ত্রুটি:", error)
        return
    }

    if let httpResponse = response as? HTTPURLResponse, (200...299).contains(httpResponse.statusCode) {
        if let mimeType = httpResponse.mimeType, mimeType == "text/html",
           let data = data, let string = String(data: data, encoding: .utf8) {
            print("ডাউনলোড করা ওয়েব পেজের কন্টেন্ট:")
            print(string)
        } else {
            print("অবৈধ MIME প্রকার বা এনকোডিং।")
        }
    } else {
        print("সার্ভার ত্রুটি সহকারে সাড়া দিয়েছে।")
    }
}
task.resume()
// নিশ্চিত করুন যে প্লেগ্রাউন্ডটি টাস্ক সম্পন্ন হওয়া অবধি চলমান থাকে
RunLoop.current.run()
```

নমুনা আউটপুট এরকম দেখাবে:

```
ডাউনলোড করা ওয়েব পেজের কন্টেন্ট:
<!doctype html>...
```

## গভীরে যান
`URLSession` API iOS 7 এবং macOS 10.9 থেকে মজুদ আছে। তা তখন খেলার মাঠ পরিবর্তন করেছিল, পুরনো, আরও জটিল `NSURLConnection` কে প্রতিস্থাপন করে। যদিও `URLSession` শক্তিশালী এবং নমনীয়, আপনি আরও জটিল নেটওয়ার্কিং প্রয়োজনীয়তা সমাধানের জন্য তৃতীয়-পক্ষের লাইব্রেরী 
 যেমন Alamofire বিবেচনা করতে পারেন।

বাস্তবায়নের সময় মনে রাখবেন যে নেটওয়ার্ক অনুরোধগুলি অ্যাসিঙ্ক্রোনাস। এর মানে হল আপনার অ্যাপ সার্ভার আপনাকে উত্তর দেওয়ার সময় অন্যান্য কাজ চালিয়ে যেতে পারে। তাছাড়া, `URLSession` সঠিকভাবে ব্যবহার করা মানে ত্রুটিগুলি সুন্দরভাবে হ্যান্ডেল করা এবং সার্ভারের সাড়া স্থিতি পরীক্ষা করা। MIME প্রকার যাচাই করা গুরুত্বপূর্ণ যাতে আপনি HTML পাচ্ছেন, JSON বা ইমেজের মতো অন্যান্য ফাইল প্রকার না।

## দেখুন এবং এক্সপ্লোর করুন
আরও গভীরে যান বা বিকল্পগুলি এক্সপ্লোর করুন:
- Apple এর `URLSession` ডকুমেন্টেশন: [URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- Alamofire সাথে Swift নেটওয়ার্কিং: [Alamofire](https://github.com/Alamofire/Alamofire)
- iOS 15+ এ জন্য `URLSession` এসিন্ক/অ্যাওয়েট প্যাটার্ন: [URLSession অ্যাসিন্ক/অ্যাওয়েট](https://developer.apple.com/videos/play/wwdc2021/10054/)
