---
title:                "Java: Надіслання http-запиту з базовою аутентифікацією."
simple_title:         "Надіслання http-запиту з базовою аутентифікацією."
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Почему

Цей блог пост написаний для тих, хто хоче навчитися відправляти HTTP-запит з базовим аутентифікацією в своїй Java програмі. Деякі веб-сервіси захищені базовою аутентифікацією, тому вміння це робити допоможе вам отримувати доступ до цих сервісів і виконувати запити.

## Як

Найпростішим способом відправити HTTP-запит з базовим аутентифікацією в Java є використання стандартного пакету `java.net` та класу `HttpURLConnection`.

Приклад коду:

```Java
import java.net.HttpURLConnection;
import java.net.URL;
import java.io.OutputStream;
import java.util.Base64;

String url = "http://example.com/api";
String username = "myusername";
String password = "mypassword";

URL obj = new URL(url);
HttpURLConnection con = (HttpURLConnection) obj.openConnection();

// Додаємо заголовки для базової аутентифікації
String auth = username + ":" + password;
String encodedAuth = Base64.getEncoder().encodeToString(auth.getBytes());
String basicAuth = "Basic " + encodedAuth;
con.setRequestProperty("Authorization", basicAuth);

// Налаштовуємо метод і параметри запиту
con.setRequestMethod("GET");
con.setDoOutput(true);

// Відправляємо запит
OutputStream os = con.getOutputStream();
os.flush();
os.close();

// Отримуємо відповідь в форматі тексту
int responseCode = con.getResponseCode();
System.out.println("Відповідь: " + responseCode + " " + con.getResponseMessage());
String response = con.getResponseMessage();

// Виводимо результат
System.out.println(response);
```

Результат виконання цього коду буде виглядати приблизно так:

```
Відповідь: 200 OK
{
  "message": "Success!"
}
```

## Заглиблення

Щоб краще зрозуміти, як саме відбувається відправка HTTP-запиту з базовою аутентифікацією, варто розглянути детальніше кожен крок.

1. Створення об'єкта `URL` з посиланням на веб-сервіс, до якого ми хочемо зробити запит.
2. Відкриття з'єднання з веб-сервісом за допомогою класу `HttpURLConnection` та методу `openConnection()`.
3. Генерування строки для базової аутентифікації шляхом об'єднання ім'я користувача та пароля через двокрапку та переведення цієї строки в формат base64 для безпечної передачі.
4. Додавання заголовка з інформацією про базову аутентифікацію до запиту за допомогою методу `setRequestProperty()`.
5. Встановлення методу та параметрів запиту за допомогою методів `setRequestMethod()` та `setDoOutput()`.
6. Завершення запиту і отримання відповіді в форматі тексту за допомогою методів `getOutputStream()` та `getResponseMessage()`.
7. Виведення результату запиту на екран.

Загалом, ві