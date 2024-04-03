---
date: 2024-01-20 18:02:02.357942-07:00
description: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E\
  \ \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u043F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456 \u0437\u0432\u0435\u0440\u0442\u0430\u0442\u0438\u0441\u044F\
  \ \u0434\u043E \u0437\u0430\u0445\u0438\u0449\u0435\u043D\u043E\u0433\u043E \u0432\
  \u0435\u0431 \u0440\u0435\u0441\u0443\u0440\u0441\u0443. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u044E\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u0437\
  \u0430\u0445\u0438\u0441\u0442\u0443 \u0434\u0430\u043D\u0438\u0445 \u0442\u0430\
  \u2026"
lastmod: '2024-03-13T22:44:49.079002-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E\
  \ \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u043F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456 \u0437\u0432\u0435\u0440\u0442\u0430\u0442\u0438\u0441\u044F\
  \ \u0434\u043E \u0437\u0430\u0445\u0438\u0449\u0435\u043D\u043E\u0433\u043E \u0432\
  \u0435\u0431 \u0440\u0435\u0441\u0443\u0440\u0441\u0443."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

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
