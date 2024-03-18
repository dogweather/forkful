---
title:                "HTML পার্স করা"
date:                  2024-03-17T18:04:17.134840-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?

প্রোগ্রামিংয়ে HTML পার্সিং মানে HTML ডকুমেন্টের গঠন বিশ্লেষণ করা, যা আপনাকে এর বিষয়বস্তু প্রোগ্রাম্যাটিক্ভাবে বের করে নেওয়া, ম্যানিপুলেশন, এবং ইন্টার‌্যাক্ট করার সুযোগ দেয়। প্রোগ্রামারগণ ওয়েব স্ক্র্যাপিং, ডেটা এক্সট্র্যাকশন বা বিভিন্ন অ্যাপ্লিকেশনের জন্য ডায়নামিক্যালি ওয়েব পেজ বা HTML ডকুমেন্টগুলি মডিফাই করতে এটি করে। এটি ওয়েব ডেভেলপমেন্ট, ডেটা বিশ্লেষণ, এবং অটোমেটেড টেস্টিং পরিস্থিতিতে একটি অপরিহার্য দক্ষতা তৈরি করে।

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
