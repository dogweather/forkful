---
title:                "HTML পার্স করা"
date:                  2024-03-17T18:04:28.340996-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?

HTML পার্স করা মানে মার্কআপের মধ্য দিয়ে ডেটা যেমন টেক্সট, লিংক, অথবা অন্যান্য উপাদান বের করে আনা। আমরা এটি ওয়েব কন্টেন্টের সাথে মিথস্ক্রিয়া করতে, ওয়েব ব্রাউজিং কার্যক্রম অটোমেট করতে, অথবা ওয়েব অ্যাপ্লিকেশন পরীক্ষা করতে করি।

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
