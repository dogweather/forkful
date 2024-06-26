---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:04:28.340996-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0986\u09B8\u09C1\u09A8 \u099C\
  \u09BE\u09AD\u09BE\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B0\u09BF\u09AF\u09BC\u09C7\
  \u09B2-\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09B2\u09CD\u09A1 HTML \u09A8\u09BF\u09AF\
  \u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09C1\u09AC\u09BF\u09A7\u09BE\u099C\u09A8\u0995\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF, Jsoup \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BF\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\
  \u09C7, \u09A8\u09BF\u09B0\u09CD\u09AD\u09B0\u09A4\u09BE\u099F\u09BF \u09AF\u09CB\
  \u0997 \u0995\u09B0\u09C1\u09A8."
lastmod: '2024-03-17T18:47:43.902094-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B8\u09C1\u09A8 \u099C\u09BE\u09AD\u09BE\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 \u09B0\u09BF\u09AF\u09BC\u09C7\u09B2-\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\
  \u09B2\u09CD\u09A1 HTML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B8\u09C1\
  \u09AC\u09BF\u09A7\u09BE\u099C\u09A8\u0995 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF, Jsoup \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BF\
  \u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u09A8\u09BF\u09B0\u09CD\u09AD\u09B0\
  \u09A4\u09BE\u099F\u09BF \u09AF\u09CB\u0997 \u0995\u09B0\u09C1\u09A8."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কিভাবে:
আসুন জাভার সাথে রিয়েল-ওয়ার্ল্ড HTML নিয়ে কাজ করার জন্য একটি সুবিধাজনক লাইব্রেরি, Jsoup ব্যবহার করি। প্রথমে, নির্ভরতাটি যোগ করুন:

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

এখন মজার অংশ। এখানে কিভাবে একটি ওয়েবপেজের শিরোনাম ধরে এবং প্রিন্ট করে:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class HtmlParser {
    public static void main(String[] args) throws IOException {
        String url = "http://example.com";
        Document doc = Jsoup.connect(url).get();
        String title = doc.title();
        System.out.println("শিরোনাম: " + title);
    }
}
```

আউটপুট:

```
শিরোনাম: Example Domain
```

সব লিংক বের করা কেমন হবে?

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... মেইন অথবা অন্য কোন মেথডের ভিতরে
Elements links = doc.select("a[href]");
for (Element link : links) {
    System.out.println("লিংক: " + link.attr("href"));
}
```

## গভীরে ডাইভ
একসময়, HTML কে রেগেক্স প্যাটার্নের দ্বারা নিয়ন্ত্রণ করা হত, একটি পদ্ধতি যা জটিল ডকুমেন্টের জন্য ত্রুটি প্রবণ এবং দুঃস্বপ্নের। শেষ দশকের দিকে Jsoup এসেছিল, জাভা জন্য একটি jQuery-এর মত ইন্টারফেস প্রদান করে, HTML পার্স, পরিভ্রমণ এবং প্রতিষ্ঠিতি করার সুবিধা দিয়েছিল।

Jsoup একমাত্র পছন্দ নয়। জাভাস্ক্রিপ্ট সমর্থন সহ পূর্ণাঙ্গ ওয়েব অ্যাপ পরীক্ষার জন্য HtmlUnit আছে, কিন্তু এটি আরো ভারী এবং জটিল। হালকা কাজের জন্য, URL বের করা জন্য Apache Commons Validator দারুণ।

অভ্যন্তরে, Jsoup একটি DOM পার্সার ব্যবহার করে, যা সম্পূর্ণ ডকুমেন্টকে মেমোরিতে একটি গাছ হিসাবে মডেল করে। এই পদ্ধতিটি HTML কাঠামো নির্বাচন এবং নেভিগেশন খুব সহজ করে তোলে। এছাড়াও, এটি অগোছালো HTML নিয়ে ভালভাবে কাজ করে, উড়ে যাওয়া সমস্যা গুলি মেরামতি করে, নিশ্চিত করে নির্ভরযোগ্য পার্সিং।

মনে রাখবেন, যখন স্ক্রেপিং করবেন, সাইটের `robots.txt` এবং সার্ভিস শর্তাদি প্রতিরক্ষা করে আইনী বিপদ অথবা IP-ব্যান এড়িয়ে চলতে সবসময় চেক করুন।

## আরও দেখুন
- Jsoup অফিসিয়াল ডকুমেন্টেশন: https://jsoup.org/
- HtmlUnit: http://htmlunit.sourceforge.net/
- Apache Commons Validator: https://commons.apache.org/proper/commons-validator/
