---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
date:                  2024-01-20T18:02:02.357942-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Відправка HTTP-запиту з базовою автентифікацією дозволяє програмі звертатися до захищеного веб ресурсу. Програмісти використовують це для захисту даних та контролю доступу.

## Як це зробити:

Java дозволяє відправляти HTTP-запити з базовою автентифікацією використовуючи стандартний API. Для прикладу використаємо `java.net.HttpURLConnection`.

```Java
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.URL;
import java.net.HttpURLConnection;
import java.util.Base64;

public class BasicAuth {
    public static void main(String[] args) {
        try {
            String webPage = "http://example.com/api/data";
            String name = "user";
            String password = "passwd";

            String authString = name + ":" + password;
            String encodedAuthString = Base64.getEncoder().encodeToString(authString.getBytes());

            URL url = new URL(webPage);
            HttpURLConnection urlConnection = (HttpURLConnection) url.openConnection();
            urlConnection.setRequestProperty("Authorization", "Basic " + encodedAuthString);

            // Handle the response as necessary
            System.out.println("Response code: " + urlConnection.getResponseCode());
            // Other response handling code goes here

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Якщо все в порядку, ми отримаємо:
```
Response code: 200
```

## Поглиблене вивчення

Базова автентифікація, як спосіб захисту доступу, існує вже багато років. Вона проста в реалізації, але не найбезпечніша - навіть із кодуванням в Base64, з'являється ризик при перехопленні трафіку. Тому її рекомендують використовувати з HTTPS.

Є альтернативи, такі як OAuth та JWT (JSON Web Tokens), які забезпечують більшу безпеку завдяки токенах доступу.

Розуміння, як влаштована базова автентифікація у HTTP, важливе, щоб вибудувати коректний зв'язок між клієнтом і сервером та розуміти, як захистити свої API.

## Дивіться також

- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Офіційна документація Oracle для класу HttpURLConnection](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/net/HttpURLConnection.html)
- [Посібник з Основоположних Принципів Безпеки для Розробників від OWASP](https://owasp.org/www-project-top-ten/)