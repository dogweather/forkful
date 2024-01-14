---
title:                "Java: Відправлення запиту http"
simple_title:         "Відправлення запиту http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому варто використовувати HTTP запити

У сучасному світі, де інтернет став необхідною частиною нашого життя, HTTP запити є ключовим інструментом для взаємодії з веб-сайтами та додатками. Завдяки ним ми можемо передавати дані до сервера та отримувати відповіді від нього. Використання HTTP запитів є необхідним для розробки веб-додатків та налагодження мережевих з'єднань.

## Як відправити HTTP запит у Java

Для відправлення HTTP запиту у Java нам потрібно використати клас `HttpURLConnection`, який надається Java API. Спочатку ми повинні використати метод `openConnection ()` для створення з'єднання з URL, на який хочемо надіслати запит. Потім ми можемо налаштувати наш запит, встановивши метод (GET, POST, PUT, DELETE), передаючи параметри та встановлюючи заголовки. Наприклад, для відправлення GET запиту до веб-сайту "example.com", ми можемо використати наступний код:

```Java
URL url = new URL("http://www.example.com");
HttpURLConnection con = (HttpURLConnection) url.openConnection();

con.setRequestMethod("GET");

int responseCode = con.getResponseCode();
System.out.println("Response Code: " + responseCode);

BufferedReader in = new BufferedReader( new InputStreamReader(con.getInputStream()));
String inputLine;
StringBuffer response = new StringBuffer();

while ((inputLine = in.readLine()) != null) {
    response.append(inputLine);
}
in.close();

System.out.println(response.toString());
```

Видавшийся результат буде містити код сторінки `"example.com"`.

## Глибші занурення в HTTP запити

У вищевказаному прикладі ми відправляємо простий GET запит та отримуємо відповідь у вигляді сторінки. Однак, HTTP запити мають багато можливостей та функцій. Наприклад, ми можемо встановити параметри запиту, включити аутентифікацію, обмежити час очікування відповіді та багато іншого.

## Дивись також

Якщо ви хочете дізнатися більше про роботу з HTTP запитами у Java, ось деякі корисні ресурси:

- [Документація Java для класу HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Відеоуроки Java Tutorials - Working with URLs](https://www.youtube.com/watch?v=_x2Q3lbufJc)
- [Стаття "Робота з HTTP запитами у Java" на сайті Baeldung](https://www.baeldung.com/java-http-request)