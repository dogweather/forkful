---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:09.092662-07:00
description: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0434\u043E\u0431\u0430\u0432\
  \u043B\u0435\u043D\u0438\u0435 \u0437\u0430\u0433\u043E\u043B\u043E\u0432\u043A\u0430\
  \ \u0441 \u0438\u043C\u0435\u043D\u0435\u043C \u043F\u043E\u043B\u044C\u0437\u043E\
  \u0432\u0430\u0442\u0435\u043B\u044F \u0438 \u043F\u0430\u0440\u043E\u043B\u0435\
  \u043C \u0434\u043B\u044F \u0434\u043E\u0441\u0442\u0443\u043F\u0430 \u043A \u0437\
  \u0430\u0449\u0438\u0449\u0435\u043D\u043D\u043E\u043C\u0443 \u0440\u0435\u0441\u0443\
  \u0440\u0441\u0443.\u2026"
lastmod: '2024-03-13T22:44:44.820088-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0434\u043E\u0431\u0430\u0432\
  \u043B\u0435\u043D\u0438\u0435 \u0437\u0430\u0433\u043E\u043B\u043E\u0432\u043A\u0430\
  \ \u0441 \u0438\u043C\u0435\u043D\u0435\u043C \u043F\u043E\u043B\u044C\u0437\u043E\
  \u0432\u0430\u0442\u0435\u043B\u044F \u0438 \u043F\u0430\u0440\u043E\u043B\u0435\
  \u043C \u0434\u043B\u044F \u0434\u043E\u0441\u0442\u0443\u043F\u0430 \u043A \u0437\
  \u0430\u0449\u0438\u0449\u0435\u043D\u043D\u043E\u043C\u0443 \u0440\u0435\u0441\u0443\
  \u0440\u0441\u0443."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

## Как это сделать:
Java делает достаточно простым отправку HTTP-запросов с базовой аутентификацией с использованием класса `HttpURLConnection`. Вот быстрый пример:

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
            System.out.println("Код ответа: " + responseCode);

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
                System.out.println("GET запрос не сработал");
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
Пример вывода:
```
Код ответа: 200
{ "message": "Это ответ от защищенного ресурса!" }
```

## Подробнее
Базовая аутентификация существует с первых дней HTTP. Она работает, передавая учетные данные, закодированные в base64, в заголовке, что делает ее простой, но не очень безопасной без HTTPS, поскольку учетные данные могут быть легко декодированы.

Альтернативы, такие как OAuth, добавляют дополнительный уровень безопасности, используя токены. Аутентификация на основе токенов предпочтительнее в настоящее время, особенно для RESTful API.

При реализации базовой аутентификации доступа на Java с рекомендуемым способом с Java 11 является использование нового класса `HttpClient`. Он более универсален и поддерживает HTTP/2 из коробки. Тем не менее, для базовых требований или устаревших систем `HttpURLConnection` остается жизнеспособным вариантом.

## Смотрите также
- [RFC 7617 - Схема аутентификации 'Basic' в HTTP](https://tools.ietf.org/html/rfc7617)
- [Документация по API клиента HTTP Java 11 от Oracle](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [Руководство Baeldung по HTTP-запросам в Java](https://www.baeldung.com/java-http-request)
