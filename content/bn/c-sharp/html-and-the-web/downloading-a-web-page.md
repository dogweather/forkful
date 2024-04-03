---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:36.668179-06:00
description: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\
  \u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0995\u09CB\
  \u09A1 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0987\u09A8\
  \u09CD\u099F\u09BE\u09B0\u09A8\u09C7\u099F \u09A5\u09C7\u0995\u09C7 \u09B0 \u09B9\
  \u09BF\u099F\u09BF\u098F\u09AE\u098F\u09B2 \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\
  \u099F \u09A8\u09C7\u0993\u09AF\u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09A5\u09CD\u09AF \u09AA\
  \u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09B0\u09A3, \u0993\
  \u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\u09B8\u09C7\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u09AF\u09CB\u0997\u09BE\u09AF\u09CB\u0997 \u0985\u09A5\
  \u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.036064-06:00'
model: gpt-4-0125-preview
summary: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09A1\u09BE\u0989\u09A8\
  \u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0995\u09CB\u09A1\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0987\u09A8\u09CD\
  \u099F\u09BE\u09B0\u09A8\u09C7\u099F \u09A5\u09C7\u0995\u09C7 \u09B0 \u09B9\u09BF\
  \u099F\u09BF\u098F\u09AE\u098F\u09B2 \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F\
  \ \u09A8\u09C7\u0993\u09AF\u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09A5\u09CD\u09AF \u09AA\u09CD\
  \u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09B0\u09A3, \u0993\u09AF\
  \u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\u09B8\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AF\u09CB\u0997\u09BE\u09AF\u09CB\u0997 \u0985\u09A5\u09AC\
  \u09BE \u0995\u09C7\u09AC\u09B2 \u0985\u09AB\u09B2\u09BE\u0987\u09A8 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09A4\u09A5\
  \u09CD\u09AF \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3 \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BE \u0995\u09B0\u09C7\u0964."
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কিভাবে:
C# এর মাধ্যমে `HttpClient` ক্লাস ব্যবহার করে ওয়েব পেজ ডাউনলোড করা সহজ হয়। এখানে একটি দ্রুত উদাহরণ দেওয়া হলো:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using (HttpClient client = new HttpClient())
        {
            try
            {
                string url = "http://example.com"; // প্রয়োজনীয় URL দিয়ে প্রতিস্থাপন করুন
                HttpResponseMessage response = await client.GetAsync(url);
                response.EnsureSuccessStatusCode();
                string responseBody = await response.Content.ReadAsStringAsync();
                
                Console.WriteLine(responseBody); // র হিটিএমএল কন্টেন্ট আউটপুট করে
            }
            catch (HttpRequestException e)
            {
                Console.WriteLine("\nException Caught!");
                Console.WriteLine("Message :{0} ", e.Message);
            }
        }
    }
}
```

এটি নির্দিষ্ট ওয়েব পেজের হিটিএমএল কন্টেন্ট কনসোলে আউটপুট করবে।

## গভীরে যাওয়া
`HttpClient` ব্যবহারের আগে, C# `WebClient` এবং `HttpWebRequest` এর মত ক্লাস ব্যবহার করত ওয়েব কন্টেন্ট ডাউনলোড করার জন্য। `HttpClient` হল সর্বশেষ এবং এটি পুনঃব্যবহারযোগ্য, দক্ষ এবং অ্যাসিঙ্ক্রোনাস কার্যক্রমগুলির সমর্থন করার জন্য ডিজাইন করা হয়েছে যা নতুন অ্যাপ্লিকেশনগুলির জন্য পছন্দের বিকল্প হয়ে উঠেছে।

বিকল্পও রয়েছে। যেমন, `HtmlAgilityPack` মত থার্ড-পার্টি লাইব্রেরি এইচটিএমএল পার্স করতে পারে, ডম নেভিগেট করা বা র হিটিএমএল স্ট্রিং নিয়ে ঝামেলা না করে নির্দিষ্ট তথ্য অন্যান্য বিষয় আহরণ করতে সহজ করে তোলে।

ওয়েব পেজগুলি ডাউনলোড করার সময়, মনে রাখবেন: robots.txt ফাইলগুলিকে সম্মান করুন, ব্যতিক্রমগুলি সামাল দিন, এবং ওয়েবসাইটের ব্যবহারের শর্তাবলী সম্পর্কে সচেতন থাকুন।

## আরও দেখুন
- [HttpClient ক্লাস ডকুমেন্টেশন](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Async এবং Await](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/)
- [HTML Agility Pack on GitHub](https://github.com/zzzprojects/html-agility-pack)
- [robots.txt সম্মান](https://developers.google.com/search/docs/advanced/robots/intro)
