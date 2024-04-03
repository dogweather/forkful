---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:04:17.134840-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: .NET \u0993\u09AF\u09BC\u09C7\u09AC\
  \ \u09AA\u09C7\u099C \u09AB\u09C7\u099A \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF `HttpClient` \u098F\u09B0 \u09AE\u09A4\u09CB HTML \u09A8\u09BF\u09AF\u09BC\
  \u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u09AC\u09C7\u09B8\u09BF\u0995\
  \ \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8\
  \ \u0995\u09B0\u09C7, \u09A4\u09AC\u09C7 \u09A4\u09BE \u09A8\u09BF\u099C\u09B8\u09CD\
  \u09AC \u0995\u09CB\u09A8\u09CB \u09B8\u09AE\u09CD\u09AA\u09C2\u09B0\u09CD\u09A3\
  \ HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BE\u09B0 \u09A8\u09C7\u0987\u0964 \u09A4\
  \u09BE\u0987,\u2026"
lastmod: '2024-03-17T18:47:44.035054-06:00'
model: gpt-4-0125-preview
summary: ".NET \u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C \u09AB\u09C7\u099A\
  \ \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF `HttpClient` \u098F\u09B0 \u09AE\
  \u09A4\u09CB HTML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE\u09B0 \u09AC\u09C7\u09B8\u09BF\u0995 \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, \u09A4\u09AC\u09C7\
  \ \u09A4\u09BE \u09A8\u09BF\u099C\u09B8\u09CD\u09AC \u0995\u09CB\u09A8\u09CB \u09B8\
  \u09AE\u09CD\u09AA\u09C2\u09B0\u09CD\u09A3 HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BE\
  \u09B0 \u09A8\u09C7\u0987\u0964 \u09A4\u09BE\u0987, \u0985\u09A7\u09BF\u0995\u09BE\
  \u0982\u09B6 C# \u09A1\u09C7\u09AD\u09C7\u09B2\u09AA\u09BE\u09B0\u09B0\u09BE \u09B0\
  \u09CB\u09AC\u09BE\u09B8\u09CD\u099F HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982\
  \ \u0995\u09CD\u09B7\u09AE\u09A4\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u099C\u09A8\
  \u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\
  \u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \ \u09AF\u09C7\u09AE\u09A8 HtmlAgilityPack \u09AC\u09BE AngleSharp \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u09A8\u0964 \u0989\u09AD\u09AF\u09BC\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0987 HTML DOM-\u098F \u09B8\
  \u09B9\u099C \u0995\u09CB\u09AF\u09BC\u09C7\u09B0\u09BF, \u09AE\u09CD\u09AF\u09BE\
  \u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u09B6\u09A8, \u098F\u09AC\u0982 \u09AD\u09CD\
  \u09B0\u09AE\u09A3\u09C7 \u09B8\u09BE\u09B9\u09BE\u09AF\u09CD\u09AF \u0995\u09B0\
  \u09C7\u0964\n\n#."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কিভাবে:
.NET ওয়েব পেজ ফেচ করার জন্য `HttpClient` এর মতো HTML নিয়ে কাজ করার বেসিক সাপোর্ট প্রদান করে, তবে তা নিজস্ব কোনো সম্পূর্ণ HTML পার্সার নেই। তাই, অধিকাংশ C# ডেভেলপাররা রোবাস্ট HTML পার্সিং ক্ষমতার জন্য জনপ্রিয় তৃতীয় পক্ষের লাইব্রেরি যেমন HtmlAgilityPack বা AngleSharp ব্যবহার করেন। উভয় লাইব্রেরিই HTML DOM-এ সহজ কোয়েরি, ম্যানিপুলেশন, এবং ভ্রমণে সাহায্য করে।

### HtmlAgilityPack ব্যবহার করে
1. **HtmlAgilityPack ইন্সটল করুন**: প্রথমে, নুগেটের মাধ্যমে আপনার প্রকল্পে HtmlAgilityPack প্যাকেজ যোগ করুন।
   ```
   Install-Package HtmlAgilityPack
   ```

2. **উদাহরণ কোড**: একটি HTML স্ট্রিং পার্স করে, সমস্ত `<h1>` এলিমেন্টের শিরোনাম বের করুন।

   ```csharp
   using HtmlAgilityPack;
   using System;
   using System.Linq;

   class Program
   {
       static void Main(string[] args)
       {
           var html = @"<html>
                         <body>
                             <h1>Title 1</h1>
                             <h1>Title 2</h1>
                         </body>
                        </html>";
           var htmlDoc = new HtmlDocument();
           htmlDoc.LoadHtml(html);

           var h1Tags = htmlDoc.DocumentNode.SelectNodes("//h1").Select(node => node.InnerText);
           foreach (var title in h1Tags)
           {
               Console.WriteLine(title);
           }
       }
   }
   ```

   **নমুনা আউটপুট:**
   ```
   Title 1
   Title 2
   ```

### AngleSharp ব্যবহার করে
1. **AngleSharp ইন্সটল করুন**: নুগেটের মাধ্যমে আপনার প্রকল্পে AngleSharp লাইব্রেরিটি যোগ করুন।
   ```
   Install-Package AngleSharp
   ```

2. **উদাহরণ কোড**: একটি HTML ডকুমেন্ট লোড করে এবং বিশেষ ক্লাস সহ `div` এলিমেন্টস কোয়েরি করুন।

   ```csharp
   using AngleSharp;
   using AngleSharp.Dom;
   using System;
   using System.Linq;
   using System.Threading.Tasks;

   class Program
   {
       static async Task Main(string[] args)
       {
           var context = BrowsingContext.New(Configuration.Default);
           var document = await context.OpenAsync(req => req.Content("<div class='item'>Item 1</div><div class='item'>Item 2</div>"));

           var items = document.QuerySelectorAll(".item").Select(element => element.TextContent);
           foreach (var item in items)
           {
               Console.WriteLine(item);
           }
       }
   }
   ```

   **নমুনা আউটপুট:**
   ```
   Item 1
   Item 2
   ```

HtmlAgilityPack এবং AngleSharp উভয়ই HTML পার্সিংয়ের জন্য শক্তিশালী টুল, তবে আপনার পছন্দ হয়তো নির্দিষ্ট প্রজেক্টের প্রয়োজন, পারফরমেন্স বিবেচনা, অথবা API ডিজাইনে ব্যক্তিগত পছন্দের উপর নির্ভর করবে।
