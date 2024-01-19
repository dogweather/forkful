---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ЩО І ЧОМУ?

Відправлення HTTP-запиту з базовою автентифікацією - це спосіб, який дозволяє програмістам передати обмежені дані через HTTP-запит. Ми робимо це, щоб автоматизувати з’єднання з серверами та передачу даних без необхідності ручного введення облікових даних.

## ЯК ЦЕ ЗРОБИТИ:

Створимо HTTP-запит з базовою автентифікацією в Java за допомогою пакету java.net:

```Java
import java.net.URL;
import java.net.HttpURLConnection;
import java.util.Base64;

public class BasicAuth {
    public static void main(String[] args) throws Exception {

        String credentials = "username:password";
        String basicAuth = "Basic " + new String(Base64.getEncoder().encode(credentials.getBytes()));

        URL url = new URL("http://yoururl.com");
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();

        connection.setRequestProperty("Authorization", basicAuth);
        connection.setRequestMethod("GET");
        
        int responseCode = connection.getResponseCode();
        System.out.println("Response Code : " + responseCode);
        
        // Your additional code here
        
        connection.disconnect();
    }
}
```
Ви можете побачити код відповіді сервера на консолі. Plan your additional code as per your requirements.

## ГЛИБИННИЙ ЗАНУРЕННЯ:

Базова автентифікація HTTP була одним з перших методів, що з'явилися з HTTP. Це досить простий спосіб передачі облікових даних (ім’я користувача та пароль). Як альтернатива базової автентифікації, є автентифікація Digest, OAuth та інші.

Реалізація в Java включає в себе кодування ім’я користувача та паролю в Base64 та передачу їх через заголовок `Authorization`. Однак цей метод не є безпечним, якщо не використовується захищене з’єднання, оскільки облікові дані можуть бути легко розкодовані.

## ДИВІТЬСЯ ТАКОЖ:

1. [Java Networking Tutorial](https://docs.oracle.com/javase/tutorial/networking/overview/networking.html)
2. [Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
3. [Basic Authentication scheme](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)