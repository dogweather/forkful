---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:19:48.576860-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ Swift \u098F \u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\
  \u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 HTTP \u0985\u09A8\
  \u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB\u09B0 \u09AA\u09A6\
  \u09CD\u09A7\u09A4\u09BF \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\u09B2."
lastmod: '2024-03-17T18:47:44.409306-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 Swift \u098F \u09AC\u09C7\u09B8\u09BF\u0995\
  \ \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\
  \u09A0\u09BE\u09A8\u09CB\u09B0 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09A6\u09C7\
  \u0993\u09DF\u09BE \u09B9\u09B2."
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
weight: 45
---

## কিভাবে:
এখানে Swift এ বেসিক অথেন্টিকেশনের সাথে HTTP অনুরোধ পাঠানোর পদ্ধতি দেওয়া হল:

```Swift
import Foundation

// আপনার API এন্ডপয়েন্ট
let url = URL(string: "https://example.com/api/data")!

// আপনার প্রমাণপত্র
let username = "user"
let password = "password"

// লগ-ইন ডেটা তৈরি করে বেস64 স্ট্রিং এ পরিণত করুন
let loginData = String(format: "%@:%@", username, password).data(using: String.Encoding.utf8)!
let base64LoginData = loginData.base64EncodedString()

// অনুরোধ তৈরি করুন
var request = URLRequest(url: url)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginData)", forHTTPHeaderField: "Authorization")

// অনুরোধ পাঠান
let session = URLSession.shared
let dataTask = session.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Error: \(error)") // ত্রুটি হ্যান্ডেল করুন
    } else if let data = data, let string = String(data: data, encoding: .utf8) {
        print("Response: \(string)") // সাড়া হ্যান্ডেল করুন
    }
}

dataTask.resume()
```

আউটপুট হবে API থেকে ফেরত আসা ডেটা, অথবা কিছু ভুল হলে একটি ত্রুটি বার্তা।

## গভীরে যাওয়া
প্রাথমিক ওয়েব যুগে, বেসিক অথেন্টিকেশন ছিল সম্পদগুলিকে সুরক্ষিত করার একটি দ্রুত পদ্ধতি। এর সাধারণতা এটিকে কম নিরাপদ হওয়া সত্ত্বেও OAuth চেয়ে, কারন পরিচয়দানগুলি এনক্রিপ্ট করা না হয়, শুধু এনকোড করা হয়, ব্যাপকভাবে গ্রহণ করা হয়।

বেসিক অথেন্টিকেশন ছাড়াও, বিকল্পগুলোর মধ্যে আছে ডাইজেস্ট অথেন্টিকেশন, API কী, OAuth, অথবা JWT (JSON ওয়েব টোকেনস)। প্রতিটির নিরাপত্তা, ব্যবহারের সুবিধা, এবং অফার করা সুরক্ষা স্তরের চারপাশে সুবিধা এবং অসুবিধা রয়েছে।

বেসিক অথেন্টিকেশনের সাথে HTTP অনুরোধ পাঠানোর সময়, এটি সেরা প্র্যাকটিস হলো আপনি HTTPS ব্যবহার করা যাতে আপনার এনকোডেড প্রমাণপত্রগুলি নিরাপদে সম্প্রেষিত হয়। এছাড়া, প্রমাণপত্র হার্ডকোডিং এড়িয়ে চলা উচিত; এর বদলে, পরিবেশ ভেরিয়েবল বা সুরক্ষিত ভল্টগুলি ব্যবহার করুন।

## আরো দেখুন
- [Apple's URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP বেসিক অথেন্টিকেশন RFC](https://tools.ietf.org/html/rfc7617)
- [OAuth 2.0](https://oauth.net/2/)
