---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:19:53.891356-06:00
description: "\u09B0\u09C1\u09AC\u09BF\u09A4\u09C7, \u09AC\u09C7\u09B8\u09BF\u0995\
  \ \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u098F\u0995\u099F\u09BF HTTP \u0985\u09A8\u09C1\u09B0\
  \u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB \u09AE\u09BE\u09A8\u09C7 \u09B9\
  \u09B2\u09CB \u0986\u09AA\u09A8\u09BE\u09B0 \u0985\u09A8\u09C1\u09B0\u09CB\u09A7\
  \ \u09B9\u09C7\u09A1\u09BE\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u09A8\u09BE\u09AE \u098F\
  \u09AC\u0982 \u09AA\u09BE\u09B8\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1 \u09AF\
  \u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3 \u098F\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.589995-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF\u09A4\u09C7, \u09AC\u09C7\u09B8\u09BF\u0995 \u0985\
  \u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u09B8\u09BE\
  \u09A5\u09C7 \u098F\u0995\u099F\u09BF HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7\
  \ \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09B9\u09C7\
  \u09A1\u09BE\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u09A8\u09BE\u09AE \u098F\u09AC\u0982\
  \ \u09AA\u09BE\u09B8\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1 \u09AF\u09C1\u0995\
  \u09CD\u09A4 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u0997\u09A3 \u098F\u099F\u09BF\u2026"
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
---

{{< edit_this_page >}}

## কি এবং কেন?

রুবিতে, বেসিক অথেন্টিকেশনের সাথে একটি HTTP অনুরোধ পাঠানো মানে হলো আপনার অনুরোধ হেডারে একটি ব্যবহারকারীর নাম এবং পাসওয়ার্ড যুক্ত করা। প্রোগ্রামারগণ এটি করেন যেসব সম্পদ ব্যবহারকারী যাচাইকরণের দাবি রাখে তা অ্যাক্সেস করার জন্য।

## কিভাবে:

বেসিক অথেন্টিকেশনের সাথে একটি HTTP অনুরোধ পাঠানোর জন্য, আপনি সাধারণত রুবির `Net::HTTP` মডিউল ব্যবহার করবেন। এখানে একটি দ্রুত উদাহরণ দেওয়া হলো:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
username = 'your_username'
password = 'your_password'

request = Net::HTTP::Get.new(uri)
request.basic_auth(username, password)

response = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(request)
}

puts response.body
```

আপনি যদি এই কোডটি বৈধ প্রমাণপত্রের সাথে চালান, তাহলে আপনি প্রতিক্রিয়ার দেহটি মুদ্রিত দেখতে পাবেন। যদি প্রমাণপত্রা অবৈধ হয়, তাহলে আপনি একটি ত্রুটি বার্তা পেতে পারেন।

## গভীর ডুব

বেসিক অথেন্টিকেশনের ওয়েব প্রটোকলে দীর্ঘ ইতিহাস রয়েছে, ইন্টারনেটের কার্যকারিতা নির্ধারণ করে এমন প্রারম্ভিক RFCগুলিতে ফিরে যায়। এটি একটি সাধারণ অ্যাক্সেস নিয়ন্ত্রণ পদ্ধতি: ব্যবহারকারীর নাম এবং পাসওয়ার্ডকে Base64 এর সাথে এনকোড করে HTTP হেডারে পাঠানো হয়।

তবে, বেসিক অথেন্টিকেশন HTTP-র উপরে প্রমাণপত্রাগুলি প্লেইনটেক্সটে (যদিও এনকোড করা) প্রেরণ করে, তাই এটি নিরাপদ নয়। প্রমাণপত্রাগুলি অন্যের চোখ থেকে নিরাপদ রাখার জন্য HTTPS ব্যবহার করা ভালো।

আরও নিরাপদ বিকল্প যেমন OAuth আছে, যা প্রায়ই API অথেন্টিকেশনের জন্য ব্যবহৃত হয়। OAuth ব্যবহারকারীদের তৃতীয়-পক্ষের অ্যাক্সেসের অনুমতি দেয় ব্যক্তিগত প্রমাণপত্রা ভাগ না করে এইভাবে। তবু, বেসিক অথেন্টিকেশন ব্যবহারে বেঁচে আছে, বি�্যতিগত অ্যাপ্লিকেশন এবং দ্রুত-ও-নোংরা স্ক্রিপ্টিংয়ের জন্য বিশেষ করে।

একটি বিশদ বিষয় হল যে রুবির `Net::HTTP` নেটিভভাবে Basic Auth সম্পর্কে না জানলে, আপনি যদি সরাসরি `basic_auth` পদ্ধতিটি ব্যবহার না করেন তবে তারা এটি সম্পর্কে জানে না। এছাড়াও HTTP অনুরোধ থেকে সম্ভাব্য বিস্তারিত ব্যতিক্রম এবং ত্রুটি প্রতিক্রিয়াগুলি সম্পর্কে সাবধান থাকা জরুরী।

## আরো দেখুন

- রুবি স্ট্যান্ডার্ড লাইব্রেরি `Net::HTTP` ডকুমেন্টেশন: https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- অথেন্টিকেশনের জন্য OAuth সম্পর্কে একটি ভূমিকা: https://oauth.net/2/
- রুবি এবং HTTP অনুরোধের উপর আরো: https://www.rubyguides.com/2019/08/ruby-http-request/
