---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:17:47.642636-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8 Java\
  \ 11-\u098F\u09B0 `HttpClient`, `HttpRequest`, \u098F\u09AC\u0982 `HttpResponse`\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 GET \u0985\u09A8\
  \u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u0987 \u098F\u09AC\u0982 \u0995\
  \u09BF\u099B\u09C1 \u09A1\u09C7\u099F\u09BE \u09B8\u0982\u0997\u09CD\u09B0\u09B9\
  \ \u0995\u09B0\u09BE \u09AF\u09BE\u0995."
lastmod: '2024-03-17T18:47:43.901107-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8 Java 11-\u098F\u09B0 `HttpClient`, `HttpRequest`,\
  \ \u098F\u09AC\u0982 `HttpResponse` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7 GET \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u0987\
  \ \u098F\u09AC\u0982 \u0995\u09BF\u099B\u09C1 \u09A1\u09C7\u099F\u09BE \u09B8\u0982\
  \u0997\u09CD\u09B0\u09B9 \u0995\u09B0\u09BE \u09AF\u09BE\u0995."
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

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
