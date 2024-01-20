---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що та чому?

Відправити HTTP-запит - це просто зробити запит до веб-сервера для отримання або відправлення даних. Це необхідно для взаємодії з веб-службами та API.

## Як:

Ось невеликий приклад програми на Java для відправлення GET-запиту:

```Java
import java.net.http.*;
import java.net.URI;
import java.io.IOException;

public class HTTPRequest {
    public static void main(String[] args) throws IOException, InterruptedException {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
              .uri(URI.create("http://example.com"))
              .build();

        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        System.out.println(response.body());
    }
}
```

В результаті виконання цієї програми ми отримаємо HTML-код головної сторінки `http://example.com`.

## Поглиблено:

Перший HTTP-запит був зроблений в 1989 році, коли Тім Бернерс-Lee розробляв WWW. З того часу HTTP-запити стали основою веб-комунікацій.

За лаштунками, кожен HTTP-запит має форматування й набір вказівок, як-то методи GET або POST, шляхи URI, версії протоколу та заголовки. 

Часто використовуються інші бібліотеки, такі як OkHttp або Retrofit, які надають більше функціональності та зручності порівняно з вбудованими засобами Java.

## Дивіться також:

1. Повний набір туторіалів по HTTP-запитах на Java - [link](https://www.baeldung.com/java-http-request)
2. Вивчення OkHttp - [link](https://square.github.io/okhttp/)
3. Керівництво по Retrofit - [link](https://square.github.io/retrofit/)