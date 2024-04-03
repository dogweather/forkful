---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:19:09.390415-06:00
description: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7\u09C7 \u09AC\u09C7\u09B8\u09BF\
  \u0995 \u0985\u09A5\u09C7\u09A8\u099F\u09BF\u0995\u09C7\u09B6\u09A8 \u09B8\u09B9\
  \ \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09AA\
  \u09CD\u09B0\u09CB\u099F\u09C7\u0995\u09CD\u099F\u09C7\u09A1 \u09B0\u09BF\u09B8\u09CB\
  \u09B0\u09CD\u09B8\u09C7 \u0985\u09CD\u09AF\u09BE\u0995\u09CD\u09B8\u09C7\u09B8\
  \ \u09AA\u09C7\u09A4\u09C7 \u09B9\u09C7\u09A1\u09BE\u09B0\u09C7 \u098F\u0995\u099F\
  \u09BF \u0987\u0989\u099C\u09BE\u09B0\u09A8\u09C7\u09AE \u098F\u09AC\u0982 \u09AA\
  \u09BE\u09B8\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1 \u09AF\u09CB\u0997 \u0995\
  \u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u2026"
lastmod: '2024-03-17T18:47:43.904050-06:00'
model: gpt-4-0125-preview
summary: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7\u09C7 \u09AC\u09C7\u09B8\u09BF\
  \u0995 \u0985\u09A5\u09C7\u09A8\u099F\u09BF\u0995\u09C7\u09B6\u09A8 \u09B8\u09B9\
  \ \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09AA\
  \u09CD\u09B0\u09CB\u099F\u09C7\u0995\u09CD\u099F\u09C7\u09A1 \u09B0\u09BF\u09B8\u09CB\
  \u09B0\u09CD\u09B8\u09C7 \u0985\u09CD\u09AF\u09BE\u0995\u09CD\u09B8\u09C7\u09B8\
  \ \u09AA\u09C7\u09A4\u09C7 \u09B9\u09C7\u09A1\u09BE\u09B0\u09C7 \u098F\u0995\u099F\
  \u09BF \u0987\u0989\u099C\u09BE\u09B0\u09A8\u09C7\u09AE \u098F\u09AC\u0982 \u09AA\
  \u09BE\u09B8\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1 \u09AF\u09CB\u0997 \u0995\
  \u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\
  \u09CD\u09AD\u09BF\u09B8\u09C7 \u09B8\u09BF\u09AE\u09CD\u09AA\u09B2 \u0985\u09A5\
  \u09B0\u09BE\u0987\u099C\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u09A8, \u09AF\u0996\
  \u09A8 \u0986\u09B0\u0993 \u0989\u09A8\u09CD\u09A8\u09A4 \u09AE\u09C7\u09A5\u09A1\
  \u09C7\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09B9\u09AF\u09BC\
  \ \u09A8\u09BE\u0964."
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
weight: 45
---

## কিভাবে:
জাভাতে `HttpURLConnection` ক্লাস ব্যবহার করে বেসিক অথেনটিকেশন সহ HTTP অনুরোধ পাঠানো খুবই সহজ। এখানে একটি দ্রুত উদাহরণ দেওয়া হল:

```java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

public class BasicAuthRequest {

    public static void main(String[] args) {
        try {
            URL url = new URL("http://example.com/resource");
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            
            String userCredentials = "user:password";
            String basicAuth = "Basic " + new String(Base64.getEncoder().encode(userCredentials.getBytes(StandardCharsets.UTF_8)));
            connection.setRequestProperty("Authorization", basicAuth);

            int responseCode = connection.getResponseCode();
            System.out.println("উত্তর কোড: " + responseCode);

            if (responseCode == HttpURLConnection.HTTP_OK) {
                BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                String inputLine;
                StringBuilder response = new StringBuilder();

                while ((inputLine = in.readLine()) != null) {
                    response.append(inputLine);
                }
                in.close();

                System.out.println(response.toString());
            } else {
                System.out.println("GET অনুরোধ সঠিক ভাবে কাজ করেনি");
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
নমুনা আউটপুট:
```
উত্তর কোড: 200
{ "message": "এটি একটি প্রোটেক্টেড রিসোর্স থেকে আসা উত্তর!" }
```

## গভীরে ডুব দিন
বেসিক অথেনটিকেশন HTTPর প্রারম্ভিক দিনগুলো থেকে চলে আসছে। এটি হেডারে base64-এনকোডেড ক্রেডেনশিয়াল প্রেরণ করে, যা এটিকে সহজ কিন্তু HTTPS ছাড়া খুব বেশি নিরাপদ নয়, কারণ ক্রেডেনশিয়ালগুলি সহজেই ডিকোড করা যেতে পারে।

OAuth-এর মতো বিকল্পগুলি টোকেন ব্যবহার করে আরেকটি স্তরের নিরাপত্তা যোগ করে। টোকেন-ভিত্তিক অথেনটিকেশন বর্তমানে আগ্রহী, বিশেষ করে RESTful APIs এর জন্য।

জাভাতে বেসিক অ্যাক্সেস অথেনটিকেশন বাস্তবায়নের জন্য, জাভা 11 থেকে নতুন `HttpClient` ক্লাস ব্যবহার করা সুপারিশ করা হয়। এটি আরও বহুমুখী এবং HTTP/2 কে সরাসরি সাপোর্ট করে। তবে বেসিক চাহিদা বা লেগাসি সিস্টেমের জন্য, `HttpURLConnection` এখনও একটি বলবান বিকল্প বাকি আছে।

## আরও দেখুন
- [RFC 7617 - 'বেসিক' HTTP অথেনটিকেশন স্কিম](https://tools.ietf.org/html/rfc7617)
- [অরাকল জাভা 11 HTTP ক্লায়েন্ট API ডকুমেন্টেশন](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [ব্য্যালডুং গাইড অন জাভা HTTP অনুরোধ](https://www.baeldung.com/java-http-request)
