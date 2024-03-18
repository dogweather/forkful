---
title:                "বেসিক অথেন্টিকেশন সহ HTTP রিকুয়েস্ট প্রেরণ"
date:                  2024-03-17T18:19:09.390415-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
HTTP অনুরোধে বেসিক অথেনটিকেশন সহ পাঠানো মানে হল প্রোটেক্টেড রিসোর্সে অ্যাক্সেস পেতে হেডারে একটি ইউজারনেম এবং পাসওয়ার্ড যোগ করা। প্রোগ্রামাররা এটি ওয়েব সার্ভিসে সিম্পল অথরাইজেশনের জন্য ব্যবহার করেন, যখন আরও উন্নত মেথডের প্রয়োজন হয় না।

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
