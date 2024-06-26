---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:52.252092-07:00
description: "\u041A\u0430\u043A: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0432\
  \u043E\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C\u0441\u044F `HttpClient`,\
  \ `HttpRequest` \u0438 `HttpResponse` \u0438\u0437 Java 11, \u0447\u0442\u043E\u0431\
  \u044B \u0441\u0434\u0435\u043B\u0430\u0442\u044C GET-\u0437\u0430\u043F\u0440\u043E\
  \u0441 \u0438 \u0437\u0430\u043F\u043E\u043B\u0443\u0447\u0438\u0442\u044C \u043D\
  \u0435\u043A\u043E\u0442\u043E\u0440\u044B\u0435 \u0434\u0430\u043D\u043D\u044B\u0435\
  ."
lastmod: '2024-03-13T22:44:44.814392-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0432\u043E\u0441\u043F\u043E\
  \u043B\u044C\u0437\u0443\u0435\u043C\u0441\u044F `HttpClient`, `HttpRequest` \u0438\
  \ `HttpResponse` \u0438\u0437 Java 11, \u0447\u0442\u043E\u0431\u044B \u0441\u0434\
  \u0435\u043B\u0430\u0442\u044C GET-\u0437\u0430\u043F\u0440\u043E\u0441 \u0438 \u0437\
  \u0430\u043F\u043E\u043B\u0443\u0447\u0438\u0442\u044C \u043D\u0435\u043A\u043E\u0442\
  \u043E\u0440\u044B\u0435 \u0434\u0430\u043D\u043D\u044B\u0435."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

## Как:
Давайте воспользуемся `HttpClient`, `HttpRequest` и `HttpResponse` из Java 11, чтобы сделать GET-запрос и заполучить некоторые данные:

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

Запустите его, и вуаля — ответ сервера прямо в вашей консоли.

## Подробнее
До Java 11 отправка HTTP-запроса была более сложным процессом, который часто включал использование сторонних библиотек, таких как Apache HttpClient. `HttpURLConnection` также был вариантом, но казался динозавром — громоздким и менее интуитивно понятным.

С Java 11 `HttpClient` приходит на смену, упрощая процесс с помощью синхронных методов `.send` и асинхронных методов `.sendAsync`. Он реактивен и неблокирующий — это значит, что вам не нужно ждать, топоча ногами, пока он выполняет свою работу. Это соответствует современным потребностям эффективности приложений, где ожидание равно потерянному времени.

Альтернативы стандартным библиотекам Java? Библиотеки, такие как OkHttp и Retrofit, по-прежнему остаются фаворитами, когда требуются надежные функции и пользовательские настройки. И почему бы и нет? Они включают свои собственные преимущества, такие как пулинг соединений и преобразование вызовов "из коробки".

## Смотрите также
Подробнее о Java HttpClient на официальных документах Java:
- [HttpClient](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [HttpRequest](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpRequest.html)
- [HttpResponse](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpResponse.html)

Хотите исследовать что-то новое? Изучите OkHttp и Retrofit:
- [OkHttp](https://square.github.io/okhttp/)
- [Retrofit](https://square.github.io/retrofit/)
