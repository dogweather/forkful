---
title:                "Надсилання HTTP-запиту з основною аутентифікацією"
html_title:           "Java: Надсилання HTTP-запиту з основною аутентифікацією"
simple_title:         "Надсилання HTTP-запиту з основною аутентифікацією"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому
Цей процес - необхідна частина багатьох веб-додатків і служб для доступу до захищених даних та інформації. Використання базової аутентифікації дозволяє вам передавати логін та пароль у зашифрованому вигляді, тим самим забезпечуючи безпеку ваших даних.

## Як
```java
// Для відправлення HTTP-запиту з базовою аутентифікацією, спочатку необхідно створити об'єкт типу HttpURLConnection
URL url = new URL("http://example.com/api");
HttpURLConnection connection = (HttpURLConnection) url.openConnection();

// Встановіть HTTP-метод і заголовок "Authorization" з логіном та паролем у форматі "Basic"
connection.setRequestMethod("GET");
String basicAuth = "Basic " + Base64.getEncoder().encodeToString("логін:пароль".getBytes());
connection.setRequestProperty("Authorization", basicAuth);

// Отримайте відповідь із запиту
int responseCode = connection.getResponseCode();

// Виведіть код відповіді та повідомлення у консолі
System.out.println("Response Code: " + responseCode);
System.out.println("Response Message: " + connection.getResponseMessage());
```

Завдяки цьому коду, ви зможете прикладати HTTP-запити з базовою аутентифікацією і обробляти отримані дані.

**Вихід:**

```java
Response Code: 200
Response Message: OK
```

## Глибоке занурення
Система базової аутентифікації використовує заголовок "Authorization" для передавання у зашифрованому вигляді логіну та паролю. Цей заголовок має формат "Basic *логін:пароль*", де *логін* та *пароль* перетворюються в шифрований формат Base64 та додаються до слова "Basic". Це забезпечує безпеку від перехоплювання у відкритому вигляді вашого логіну та паролю.

## Дивіться також
- [Документація Java для HttpURLConnection](https://docs.oracle.com/javase/10/docs/api/java/net/HttpURLConnection.html)
- [How to Use Base64 Encoding in Java](https://www.baeldung.com/java-base64-encode-and-decode)
- [Understanding Basic and Digest Authentication in Java](https://www.baeldung.com/java-basic-digest-authentication)