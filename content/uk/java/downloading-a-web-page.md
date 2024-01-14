---
title:                "Java: Завантаження веб-сторінки."
simple_title:         "Завантаження веб-сторінки."
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

"
## Чому
Завантаження веб-сторінки є важливою частиною програмування веб-додатків. Це може бути корисно для отримання необхідної інформації з Інтернету або для автоматизації процесів.

## Як зробити
Для завантаження веб-сторінки використовується клас `URL` з пакету `java.net`. Спочатку потрібно створити об'єкт URL, передавши у конструктор адресу веб-сторінки. Далі можна використовувати об'єкт для отримання потоку вводу із сторінки та зчитування її вмісту. Наприклад:

```java
import java.net.*;
import java.io.*;

public class DownloadWebPage {
    public static void main(String[] args) throws Exception {
        URL url = new URL("https://example.com");
        BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()));

        String inputLine;
        while ((inputLine = reader.readLine()) != null)
            System.out.println(inputLine);
        
        reader.close();
    }
}
```

В результаті виконання програми, буде виведено вміст сторінки на екран.

## Глибоке дослідження
Завантаження веб-сторінок також може бути корисним для виконання HTTP запитів та оброблення відповідей сервера. Для цього можна використати бібліотеку `java.net.HttpURLConnection` або `java.net.HttpsURLConnection`. Ці класи надають можливість відправляти різні типи запитів (GET, POST, PUT і т.д.) та отримувати відповіді сервера. Також можна встановлювати заголовки запиту і читати заголовки відповіді.

Наприклад:

```java
URL url = new URL("https://example.com");
HttpURLConnection con = (HttpURLConnection) url.openConnection();
con.setRequestMethod("GET"); // встановлюємо тип запиту
int responseCode = con.getResponseCode();
System.out.println("Код відповіді: " + responseCode);

BufferedReader reader = new BufferedReader(new InputStreamReader(con.getInputStream()));
String inputLine;
while ((inputLine = reader.readLine()) != null)
    System.out.println(inputLine);
reader.close();
```

Це дозволить отримати вміст сторінки, а також перевірити статус виконання запиту (наприклад, чи був успішною відповідь сервера).

## Дивись також
- [Документація Java з класом URL](https://docs.oracle.com/javase/8/docs/api/java/net/URL.html)
- [Документація Java з доступом до ресурсів по URL](https://docs.oracle.com/javase/tutorial/networking/urls/readingURL.html)
- [Документація Java з HTTPUrlConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)