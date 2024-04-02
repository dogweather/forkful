---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:18:39.833031-06:00
description: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\
  \u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0\u09C7\u09B0 \u09A6\u09B0\u099C\
  \u09BE\u09AF\u09BC \u099F\u09CB\u0995\u09BE \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\
  , \u09A1\u09BE\u099F\u09BE \u099A\u09BE\u0993\u09AF\u09BC\u09BE \u0985\u09A5\u09AC\
  \u09BE \u0995\u09BF\u099B\u09C1 \u09A1\u09BE\u099F\u09BE \u09AA\u09CD\u09B0\u09A6\
  \u09BE\u09A8 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE API-\u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0987\
  \u09A8\u09CD\u099F\u09BE\u09B0\u098F\u09CD\u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF,\u2026"
lastmod: '2024-03-17T18:47:44.406171-06:00'
model: gpt-4-0125-preview
summary: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\
  \u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0\u09C7\u09B0 \u09A6\u09B0\u099C\
  \u09BE\u09AF\u09BC \u099F\u09CB\u0995\u09BE \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\
  , \u09A1\u09BE\u099F\u09BE \u099A\u09BE\u0993\u09AF\u09BC\u09BE \u0985\u09A5\u09AC\
  \u09BE \u0995\u09BF\u099B\u09C1 \u09A1\u09BE\u099F\u09BE \u09AA\u09CD\u09B0\u09A6\
  \u09BE\u09A8 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE API-\u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0987\
  \u09A8\u09CD\u099F\u09BE\u09B0\u098F\u09CD\u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF,\u2026"
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

## কি এবং কেন?

HTTP অনুরোধ পাঠানো মানে একটি ওয়েব সার্ভারের দরজায় টোকা দেওয়া, ডাটা চাওয়া অথবা কিছু ডাটা প্রদান করা। প্রোগ্রামাররা API-এর সাথে ইন্টারএ্যাক্ট করার জন্য, কনটেন্ট ডাউনলোড করার জন্য অথবা অন্যান্য সেবাসমূহের সাথে যোগাযোগ করার জন্য এটি করে থাকে।

## কিভাবে:

Swift ব্যবহার করে `URLSession` ক্লাস ব্যবহার করে HTTP অনুরোধ পাঠানো সহজ। এখানে একটি সিম্পল GET অনুরোধের উদাহরণ দেওয়া হোল:

```Swift
import Foundation

// আপনি যে রিসোর্সের URL অনুরোধ করছেন
if let url = URL(string: "https://api.example.com/data") {

    // একটি URLSessionDataTask তৈরি করুন
    let task = URLSession.shared.dataTask(with: url) { data, response, error in
        
        // চেক করুন কোন এরর আছে কিনা
        if let error = error {
            print("ডাটা ফেচ করতে গিয়ে এরর: \(error)")
            return
        }
        
        // চেক করুন আমরা একটি বৈধ রেস্পন্স এবং ডাটা পেয়েছি কি না
        if let httpResponse = response as? HTTPURLResponse, 
           httpResponse.statusCode == 200,
           let data = data {
            
            // ডাটাকে স্ট্রিং এ রূপান্তর করে প্রিন্ট করুন
            let dataString = String(decoding: data, as: UTF8.self)
            print(dataString)
        }
    }
    // টাস্ক শুরু করুন
    task.resume()
}

// স্যাম্পল আউটপুট API থেকে ফেচ করা কনটেন্ট হবে।
```

একটি POST অনুরোধ JSON-এর সাথে পাঠাতে:

```Swift
import Foundation
import CoreFoundation

// আপনার API এন্ডপয়েন্ট
if let url = URL(string: "https://api.example.com/submit") {

    // আপনি যে ডাটা পাঠাতে চান তা প্রস্তুত করুন
    let dictionary = ["key": "value"]
    guard let jsonData = try? JSONSerialization.data(withJSONObject: dictionary) else {
        print("এরর: ডিকশনারি থেকে JSON তৈরি করা যাচ্ছে না")
        return
    }
    
    // URLRequest প্রস্তুত করুন
    var request = URLRequest(url: url)
    request.httpMethod = "POST"
    request.setValue("application/json", forHTTPHeaderField: "Content-Type")
    request.httpBody = jsonData
    
    // টাস্ক তৈরি ও শুরু করুন
    let task = URLSession.shared.dataTask(with: request) { data, response, error in
        // এখানে রেস্পন্স হ্যান্ডেল করুন
    }
    task.resume()
}

// আউটপুট সার্ভারের রেস্পন্সের উপর নির্ভর করবে। কোন স্ট্যান্ডার্ড আউটপুট নাই।
```

## গভীর ডাইভ:
HTTP অনুরোধ ওয়েব যোগাযোগের মূল খাবার। এগুলো ওয়েবের আদি দিন থেকে আছে, ডেটা বিনিময়ের একটি মানকৃত উপায় প্রদান করে।

`URLSession`-এর বিকল্পের মধ্যে তৃতীয় পক্ষের লাইব্রেরিগুলি যেমন Alamofire রয়েছে যা সিনট্যাক্স সহজ করে এবং অতিরিক্ত কার্যকারিতা যোগ করে। তবে, `URLSession` নেটওয়ার্ক কলের জন্য নেটিভ গো-টু হয়ে উঠেছে, এবং Apple এটিকে সর্বশেষ নেটওয়ার্কিং ফিচার এবং সুরক্ষা মানদণ্ডের সাথে আপডেটেড রাখে।

একটি বাস্তবায়নের বিশদ বিবরণ হল যে Swift-এ নেটওয়ার্ক অনুরোধগুলি প্রকৃতি অনুযায়ী অ্যাসিঙ্ক্রোনাস। এগুলি পটভূমিতে চলতে থাকে, অ্যাপটি সাড়াশী রাখার অনুমতি দেয়। যখন একটি রেস্পন্স ফিরে আসে, তখন একটি কমপ্লিশন হ্যান্ডলার ডাকা হয়। UI আপডেট করার সময় বিশেষ করে থ্রেড ম্যানেজমেন্ট সঠিকভাবে হ্যান্ডল করা গুরুত্বপূর্ণ, যা প্রধান থ্রেডে হতে হয়।

## আরো দেখুন:

- [URLSession | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [Swift-এ JSON নিয়ে কাজ করা](https://developer.apple.com/swift/blog/?id=37)
- [Alamofire GitHub Repository](https://github.com/Alamofire/Alamofire)
