---
date: 2024-01-20 18:00:23.801248-07:00
description: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\u044F\
  \ HTTP-\u0437\u0430\u043F\u0438\u0442\u0443 \u2013 \u0446\u0435 \u043F\u0440\u043E\
  \u0446\u0435\u0441 \u0437\u0432\u0435\u0440\u043D\u0435\u043D\u043D\u044F \u0432\
  \u0430\u0448\u043E\u0457 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0438 \u0434\
  \u043E \u0441\u0435\u0440\u0432\u0435\u0440\u0430 \u0432 \u0456\u043D\u0442\u0435\
  \u0440\u043D\u0435\u0442\u0456 \u0437 \u043C\u0435\u0442\u043E\u044E \u043E\u0442\
  \u0440\u0438\u043C\u0430\u043D\u043D\u044F \u0430\u0431\u043E \u043D\u0430\u0434\
  \u0441\u0438\u043B\u0430\u043D\u043D\u044F \u0434\u0430\u043D\u0438\u0445. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\
  \u044F\u0442\u044C \u0446\u0435, \u0449\u043E\u0431\u2026"
lastmod: '2024-02-25T18:49:46.559102-07:00'
model: gpt-4-1106-preview
summary: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\u044F\
  \ HTTP-\u0437\u0430\u043F\u0438\u0442\u0443 \u2013 \u0446\u0435 \u043F\u0440\u043E\
  \u0446\u0435\u0441 \u0437\u0432\u0435\u0440\u043D\u0435\u043D\u043D\u044F \u0432\
  \u0430\u0448\u043E\u0457 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0438 \u0434\
  \u043E \u0441\u0435\u0440\u0432\u0435\u0440\u0430 \u0432 \u0456\u043D\u0442\u0435\
  \u0440\u043D\u0435\u0442\u0456 \u0437 \u043C\u0435\u0442\u043E\u044E \u043E\u0442\
  \u0440\u0438\u043C\u0430\u043D\u043D\u044F \u0430\u0431\u043E \u043D\u0430\u0434\
  \u0441\u0438\u043B\u0430\u043D\u043D\u044F \u0434\u0430\u043D\u0438\u0445. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\
  \u044F\u0442\u044C \u0446\u0435, \u0449\u043E\u0431\u2026"
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
---

{{< edit_this_page >}}

## Що та Навіщо?

Відправлення HTTP-запиту – це процес звернення вашої програми до сервера в інтернеті з метою отримання або надсилання даних. Програмісти роблять це, щоб інтегруватися з веб-сервісами, отримати важливу інформацію або взаємодіяти з іншими системами.

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
