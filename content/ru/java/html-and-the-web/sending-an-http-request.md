---
title:                "Отправка HTTP-запроса"
aliases:
- /ru/java/sending-an-http-request.md
date:                  2024-01-29T00:02:52.252092-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Отправка HTTP-запроса означает запрос данных или действий от сервера, например, открытие веб-страницы или отправку формы. Программисты делают это для взаимодействия с веб-сервисами, API и чтобы их приложения хорошо работали с другими в интернете.

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
