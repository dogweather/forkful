---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:18:53.333337-06:00
description: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B8\u09BE\u09A5\u09C7 \u09AC\u09C7\u09B8\u09BF\u0995 \u09AA\u09CD\u09B0\
  \u09AE\u09BE\u09A3\u09C0\u0995\u09B0\u09A3\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 \u098F\u0995\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \u0995\u09BE\u09B0\u09C0\u09B0 \u09A8\u09BE\u09AE \u098F\u09AC\u0982 \u09AA\u09BE\
  \u09B8\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1 \u0993\u09AF\u09BC\u09C7\u09AC\u09C7\
  \u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AA\u09CD\u09B0\u09C7\u09B0\
  \u09A3 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC \u09AF\u09BE\u09A4\u09C7 \u09B8\u09C1\
  \u09B0\u0995\u09CD\u09B7\u09BF\u09A4 \u09B8\u09AE\u09CD\u09AA\u09A6 \u0985\u09CD\
  \u09AF\u09BE\u0995\u09CD\u09B8\u09C7\u09B8 \u0995\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.497330-06:00'
model: gpt-4-0125-preview
summary: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B8\u09BE\u09A5\u09C7 \u09AC\u09C7\u09B8\u09BF\u0995 \u09AA\u09CD\u09B0\
  \u09AE\u09BE\u09A3\u09C0\u0995\u09B0\u09A3\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 \u098F\u0995\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \u0995\u09BE\u09B0\u09C0\u09B0 \u09A8\u09BE\u09AE \u098F\u09AC\u0982 \u09AA\u09BE\
  \u09B8\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1 \u0993\u09AF\u09BC\u09C7\u09AC\u09C7\
  \u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AA\u09CD\u09B0\u09C7\u09B0\
  \u09A3 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC \u09AF\u09BE\u09A4\u09C7 \u09B8\u09C1\
  \u09B0\u0995\u09CD\u09B7\u09BF\u09A4 \u09B8\u09AE\u09CD\u09AA\u09A6 \u0985\u09CD\
  \u09AF\u09BE\u0995\u09CD\u09B8\u09C7\u09B8 \u0995\u09B0\u09BE\u2026"
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
---

{{< edit_this_page >}}

## কি এবং কেন?

HTTP অনুরোধ পাঠানো সাথে বেসিক প্রমাণীকরণের মাধ্যমে একটি ব্যবহারকারীর নাম এবং পাসওয়ার্ড ওয়েবের মাধ্যমে প্রেরণ করা হয় যাতে সুরক্ষিত সম্পদ অ্যাক্সেস করা যায়। প্রোগ্রামাররা লগইন তথ্যাদি প্রয়োজন হলে APIs বা সেবাদির সাথে মিথস্ক্রিয়া করার জন্য এর সারল্য ব্যবহার করে।

## কিভাবে:

Fish Shell এ, বেসিক অথ সাথে HTTP অনুরোধ করতে `curl` ব্যবহার করুন। `username`, `password`, এবং `the_url` প্রতিস্থাপন করুন:

```Fish Shell
set -x AUTH (echo -n "username:password" | base64)
curl -H "Authorization: Basic $AUTH" the_url
```

অথবা, এনকোডিং করার জন্য `curl` এর দায়িত্ব দিন:

```Fish Shell
curl -u username:password the_url
```

নমুনা আউটপুট এরকম হতে পারে:

```Fish Shell
HTTP/1.1 200 OK
Content-Type: application/json
{
  "message": "সফলভাবে প্রমাণীকৃত।"
}
```

## গভীরে যাই

বেসিক প্রমাণীকরণ HTTP প্রোটোকলের একটি অংশ, এটি ৯০ এর দশকের প্রারম্ভে থেকে বিদ্যমান। এটি বাস্তবায়নে সহজ হলেও, ক্রেডেনশিয়ালগুলি কেবল বেস৬৪ এনকোডেড হওয়ার কারণে এটি কম নিরাপদ। HTTPS সাহায্য করে, কিন্তু এটি নির্ভুল নয়।

বিকল্পগুলির মধ্যে অন্তর্ভুক্ত OAuth, যা ক্রেডেন্শিয়ালের পরিবর্তে টোকেন ব্যবহার করে, নিরাপত্তা স্তর যোগ করে। আরও নিরাপদতার জন্য, API কী বা JWT (JSON Web Tokens) ব্যবহার করা বিবেচনা করুন।

Fish Shell এর সাথে, আমরা `curl`, একটি শক্তিশালী টুল যা বিভিন্ন প্রোটোকল এবং প্রমাণীকরণ পদ্ধতি সমর্থন করে, তার সাথে ইন্টারফেস করছি। `-u` ফ্ল্যাগ সুবিধাজনক, তবে ক্রেডেনশিয়ালগুলি হার্ডকোডিং এড়ান; পরিবর্তে, পরিবেশ চলক বা যথাযথ অনুমতিসহ কনফিগ ফাইল ব্যবহার করুন।

## আরও দেখুন:

- cURL ডকুমেন্টেশন: https://curl.se/docs/httpscripting.html
- HTTP বেসিক অথ RFC: https://tools.ietf.org/html/rfc7617
- Fish Shell ডকুমেন্টেশন: https://fishshell.com/docs/current/index.html
- JWT বুঝতে: https://jwt.io/introduction/
