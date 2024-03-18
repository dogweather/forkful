---
title:                "HTTP অনুরোধ প্রেরণ করা"
date:                  2024-03-17T18:17:47.642636-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

HTTP অনুরোধ পাঠানো মানে হল সার্ভারের কাছ থেকে ডেটা অথবা কাজের অনুরোধ করা, যেমন ওয়েবপেজ খোলা অথবা ফর্ম পাঠানো। প্রোগ্রামাররা এটা করেন ওয়েব সার্ভিস, API-এর সাথে মিথস্ক্রিয়া করার জন্য এবং তাদের অ্যাপগুলিকে ইন্টারনেটে অন্যান্যের সাথে ভালোভাবে কাজ করানোর জন্য।

## কিভাবে:

চলুন Java 11-এর `HttpClient`, `HttpRequest`, এবং `HttpResponse` ব্যবহার করে GET অনুরোধ পাঠাই এবং কিছু ডেটা সংগ্রহ করা যাক:

```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class HttpRequestExample {
    public static void main(String[] args) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                              .uri(URI.create("http://example.com"))
                              .build();

        client.sendAsync(request, HttpResponse.BodyHandlers.ofString())
              .thenApply(HttpResponse::body)
              .thenAccept(System.out::println)
              .join();
    }
}
```

আপনি এটা চালান, এবং আশ্চর্য—সার্ভার থেকে সাড়া, সরাসরি আপনার কনসোলে।

## গভীরে ডুব:

Java 11-এর আগে, HTTP অনুরোধ পাঠানো একটি জটিল প্রক্রিয়া ছিল, যাতে প্রায়ই তৃতীয় পক্ষের লাইব্রেরি যেমন Apache HttpClient ব্যবহৃত হতো। `HttpURLConnection` ও একটি অপশন ছিল তবে এটি মনে হতো একধরণের জীবাশ্ম—অনেক বেশি জটিল এবং কম স্বতস্ফূর্ত।

Java 11-এ, `HttpClient` এসেছে, প্রক্রিয়াটি সহজ করে দিয়েছে সিঙ্ক্রোনাস `.send` এবং অ্যাসিনক্রোনাস `.sendAsync` মেথডের মাধ্যমে। এটি রিঅ্যাক্টিভ এবং নন-ব্লকিং—অর্থাৎ এটি যখন কাজ করে, তখন আপনাকে অপেক্ষা করতে হয় না। এটি আধুনিক অ্যাপ দক্ষতার চাহিদার সাথে সংলগ্ন, যেখানে অপেক্ষা মানে সময় নষ্ট।

Java-র মানক লাইব্রেরির বিকল্পগুলি? যখন শক্তিশালী বৈশিষ্ট্য এবং কাস্টম কনফিগারেশনের প্রয়োজন হয়, তখন OkHttp এবং Retrofit এখনও পছন্দের মধ্যে আছে। এবং কেন না? এগুলি নিজেদের লাভ, যেমন কানেকশন পুলিং এবং কল রূপান্তর সহজে সরবরাহ করে।

## আরও দেখুন

Java HttpClient সম্পর্কে গভীরে জানুন অফিসিয়াল Java ডকুমেন্টেশন দ্বারা:
- [HttpClient](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [HttpRequest](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpRequest.html)
- [HttpResponse](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpResponse.html)

অ্যাডভেঞ্চার চাই? OkHttp এবং Retrofit অন্বেষণ করুন:
- [OkHttp](https://square.github.io/okhttp/)
- [Retrofit](https://square.github.io/retrofit/)
