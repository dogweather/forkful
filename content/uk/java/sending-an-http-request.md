---
title:                "Надсилання HTTP-запиту"
date:                  2024-01-20T18:00:23.801248-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/sending-an-http-request.md"
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
