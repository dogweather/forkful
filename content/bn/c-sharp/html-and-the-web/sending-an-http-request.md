---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:17:07.547333-06:00
description: "\u098F\u0995\u099F\u09BF HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\
  \u09BE\u09A0\u09BE\u09A8 \u0986\u09AE\u09BE\u09A6\u09C7\u09B0 \u0993\u09AF\u09BC\
  \u09C7\u09AC\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u0997\u09C1\u09B2\u09BF \u09A1\u09C7\u099F\
  \u09BE \u09AA\u09CD\u09B0\u09BE\u09B0\u09CD\u09A5\u09A8\u09BE \u09AC\u09BE \u099C\
  \u09AE\u09BE \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u098F\u0995\u099F\u09BF\
  \ \u0989\u09AA\u09BE\u09AF\u09BC, \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\
  \u0995\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BF \u098F\u09AA\u09BF\u0986\u0987, \u09B8\u09C7\u09AC\
  \u09BE \u09AC\u09BE \u0993\u09AF\u09BC\u09C7\u09AC\u2026"
lastmod: '2024-03-17T18:47:44.034118-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\
  \u09BE\u09A0\u09BE\u09A8 \u0986\u09AE\u09BE\u09A6\u09C7\u09B0 \u0993\u09AF\u09BC\
  \u09C7\u09AC\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u0997\u09C1\u09B2\u09BF \u09A1\u09C7\u099F\
  \u09BE \u09AA\u09CD\u09B0\u09BE\u09B0\u09CD\u09A5\u09A8\u09BE \u09AC\u09BE \u099C\
  \u09AE\u09BE \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\u09B0 \u098F\u0995\u099F\u09BF\
  \ \u0989\u09AA\u09BE\u09AF\u09BC, \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\
  \u0995\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BF \u098F\u09AA\u09BF\u0986\u0987, \u09B8\u09C7\u09AC\
  \u09BE \u09AC\u09BE \u0993\u09AF\u09BC\u09C7\u09AC\u2026"
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
একটি HTTP অনুরোধ পাঠান আমাদের ওয়েবের মাধ্যমে প্রোগ্রামগুলি ডেটা প্রার্থনা বা জমা দেওয়ার একটি উপায়, এটি করে থাকে। প্রোগ্রামাররা এটি এপিআই, সেবা বা ওয়েব কনটেন্ট আকর্ষণের জন্য ব্যবহার করে থাকেন।

## কিভাবে:
C# এ HTTP অনুরোধ পাঠানো খুব সহজ `HttpClient` দিয়ে। এখানে একটি GET অনুরোধের কঙ্কাল:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using HttpClient client = new HttpClient();
        HttpResponseMessage response = await client.GetAsync("http://example.com");
        response.EnsureSuccessStatusCode();
        string responseBody = await response.Content.ReadAsStringAsync();
        
        Console.WriteLine(responseBody);
    }
}
```

নমুনা আউটপুট (সংক্ষিপ্ত):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## বিস্তারিত আলোচনা
.NET Framework 4.5 এ `HttpClient` চালু করা হয়েছে HTTP যোগাযোগকে সহজ করতে। এর আগে, আপনাকে সম্ভবত `HttpWebRequest` এবং `HttpWebResponse` ক্লাসগুলির সাথে লড়াই করতে হতো, যা অধিক জটিল ছিল।

C# এ HTTP অনুরোধ পাঠানোর অন্য উপায় রয়েছে। `RestSharp` এবং `Flurl` দুটি জনপ্রিয় তৃতীয়-পক্ষের লাইব্রেরি, যেগুলি আরও স্বাচ্ছন্দ্যময় ইন্টারফেস এবং অতিরিক্ত বৈশিষ্ট্য প্রদান করে। কিন্তু `HttpClient` সাধারণত বেশিরভাগ চাহিদার জন্য যথেষ্টই হয়।

বাস্তবায়নের দিক থেকে, `HttpClient` একাধিক অনুরোধের জন্য পুনরায় ব্যবহারের জন্য ডিজাইন করা হয়েছে। প্রতিটি অনুরোধের জন্য এটি ইনস্ট্যান্ট করা ভারি লোডের অধীনে উপলব্ধ সকেটের সংখ্যাকে শেষ করতে পারে। সবসময়, আমি বলতে চাই সবসময়, `HttpClient` ইন্সট্যান্সগুলি যথাযথভাবে বিলুপ্তি দেওয়ার বিষয়ে মনোযোগ দিন যাতে কোনো সম্পদ নষ্ট না হয়।

## দেখুন এছাড়াও
- মাইক্রোসফটের `HttpClient` ডকুমেন্টেশন: [https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- HttpClient সেরা অনুশীলন: [https://aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/](https://aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/)
- `RestSharp` এর সাথে RESTful API আন্তঃক্রিয়া: [http://restsharp.org/](http://restsharp.org/)
- `Flurl` এর সাথে ধারাবাহিক HTTP (HTTP কে ধারাবাহিক করা): [https://flurl.dev/](https://flurl.dev/)
