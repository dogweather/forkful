---
date: 2024-01-20 18:00:23.801248-07:00
description: "\u042F\u043A \u0426\u0435 \u0417\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\u044F HTTP-\u0437\
  \u0430\u043F\u0438\u0442\u0456\u0432 \u0443 Java \u2013 \u0446\u0435 \u043D\u0435\
  \ \u043D\u043E\u0432\u0438\u043D\u0430. \u0417 \u0447\u0430\u0441\u0456\u0432 J2SE\
  \ 1.4, HttpURLConnection \u0431\u0443\u043B\u0430 \u043E\u0441\u043D\u043E\u0432\
  \u043D\u0438\u043C \u043C\u0435\u0442\u043E\u0434\u043E\u043C. \u0422\u0435\u043F\
  \u0435\u0440\u0456\u0448\u043D\u044F HttpClient API (\u0437\u2026"
lastmod: '2024-04-05T22:51:02.192669-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\u044F\
  \ HTTP-\u0437\u0430\u043F\u0438\u0442\u0456\u0432 \u0443 Java \u2013 \u0446\u0435\
  \ \u043D\u0435 \u043D\u043E\u0432\u0438\u043D\u0430."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

## Як Це Зробити:
```java
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.URI;
import java.time.Duration;

public class HttpExample {
    public static void main(String[] args) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("https://api.example.com/data"))
                .timeout(Duration.ofMinutes(1))
                .header("Content-Type", "application/json")
                .GET() // або .POST(BodyPublishers.ofString(json)), для надсилання даних
                .build();

        try {
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
            System.out.println("Статус: " + response.statusCode());
            System.out.println("Тіло відповіді: " + response.body());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Вивід:
```
Статус: 200
Тіло відповіді: {"key":"value"}
```

## Поглиблено:
Відправлення HTTP-запитів у Java – це не новина. З часів J2SE 1.4, HttpURLConnection була основним методом. Теперішня HttpClient API (з Java 11 і вище) пропонує сучасний та зручніший спосіб роботи з HTTP, як у синхронному, так і в асинхронному режимах.

Альтернативи – це бібліотеки сторонніх розробників, наприклад, Apache HttpClient або OkHttp. Вони пропонують розширені можливості та гнучкість, але для багатьох стандартних задач новий HttpClient API в Java буде цілком достатнім.

Про історію: HttpClient API було представлено в Java 9 як експериментальний API (JEP 110), але стало стандартним в Java 11 (JEP 321).

## Дивіться Також:
- [HttpClient documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html) – офіційна документація.
- [JEP 110: HTTP Client API](https://openjdk.java.net/jeps/110) – Інформація про експериментальний HTTP API.
- [JEP 321: HTTP Client (Standard)](https://openjdk.java.net/jeps/321) – Інформація про стандартизацію HTTP API.
- [OkHttp](https://square.github.io/okhttp/) – потужна бібліотека для взаємодії з HTTP і HTTPS.
