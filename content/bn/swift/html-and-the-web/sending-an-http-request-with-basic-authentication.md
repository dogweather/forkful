---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:19:48.576860-06:00
description: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09AF\u09BE \u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\
  \u099F\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u09B8\u0999\u09CD\u0997\u09C7\
  \ \u099C\u09DC\u09BF\u09A4, \u098F\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\u099A\u09CD\
  \u099B\u09C7 \u098F\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u0985\u09A8\u09C1\u09B0\
  \u09CB\u09A7\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\
  \u09B0 \u09A8\u09BE\u09AE \u098F\u09AC\u0982 \u09AA\u09BE\u09B8\u0993\u09AF\u09BC\
  \u09BE\u09B0\u09CD\u09A1 \u09AF\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09BE \u09AF\
  \u09BE \u09A8\u09BF\u09DF\u09A8\u09CD\u09A4\u09CD\u09B0\u09BF\u09A4 \u0993\u09AF\
  \u09BC\u09C7\u09AC\u2026"
lastmod: '2024-03-17T18:47:44.409306-06:00'
model: gpt-4-0125-preview
summary: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09AF\u09BE \u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\
  \u099F\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u09B8\u0999\u09CD\u0997\u09C7\
  \ \u099C\u09DC\u09BF\u09A4, \u098F\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\u099A\u09CD\
  \u099B\u09C7 \u098F\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u0985\u09A8\u09C1\u09B0\
  \u09CB\u09A7\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\
  \u09B0 \u09A8\u09BE\u09AE \u098F\u09AC\u0982 \u09AA\u09BE\u09B8\u0993\u09AF\u09BC\
  \u09BE\u09B0\u09CD\u09A1 \u09AF\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09BE \u09AF\
  \u09BE \u09A8\u09BF\u09DF\u09A8\u09CD\u09A4\u09CD\u09B0\u09BF\u09A4 \u0993\u09AF\
  \u09BC\u09C7\u09AC\u2026"
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
---

{{< edit_this_page >}}

## কি এবং কেন?

HTTP অনুরোধ পাঠানো যা বেসিক অথেন্টিকেশনের সঙ্গে জড়িত, এর মানে হচ্ছে এমন একটি অনুরোধে ব্যবহারকারীর নাম এবং পাসওয়ার্ড যুক্ত করা যা নিয়ন্ত্রিত ওয়েব কন্টেন্ট এর জন্য। প্রোগ্রামাররা এটি করেন অনুমোদিত ব্যবহারকারীদের জন্য সীমাবদ্ধ API বা সম্পদে প্রবেশের জন্য।

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
