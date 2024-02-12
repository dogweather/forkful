---
title:                "Отправка HTTP-запроса с базовой аутентификацией"
aliases:
- /ru/java/sending-an-http-request-with-basic-authentication/
date:                  2024-01-29T00:03:09.092662-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса с базовой аутентификацией"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Отправка HTTP-запроса с базовой аутентификацией включает добавление заголовка с именем пользователя и паролем для доступа к защищенному ресурсу. Программисты используют ее для простой авторизации в веб-сервисах, когда более продвинутые методы не требуются.

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
