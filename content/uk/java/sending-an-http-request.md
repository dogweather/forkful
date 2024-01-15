---
title:                "Надсилання http-запиту"
html_title:           "Java: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Тому чому

Найбільш імовірний мотив для надсилання HTTP запиту - отримати доступ до ресурсів у мережі Інтернет. Це може бути потрібно для завантаження даних, відправлення запитів на отримання інформації або виконання різноманітних дій на веб-сайті. У цьому статті ми розкажемо про основний процес відправлення HTTP запиту та покажемо приклади коду, щоб ви могли легко зрозуміти, як це працює.

## Як це зробити

Для відправлення HTTP запиту в Java вам знадобиться використовувати клас `HttpURLConnection` з пакету `java.net`. Нижче наведений приклад коду, який демонструє цей процес:

```Java
// Імпортуємо необхідні класи
import java.net.HttpURLConnection;
import java.net.URL;

// Створюємо об'єкт URL з адресою запиту
URL url = new URL("https://www.example.com/api/search?query=Java");

// Встановлюємо з'єднання з ресурсом за допомогою HttpURLConnection
HttpURLConnection con = (HttpURLConnection) url.openConnection();

// Задаємо метод запиту, в даному випадку - GET
con.setRequestMethod("GET");

// Встановлюємо параметри запиту, якщо потрібно
con.setRequestProperty("Authorization", "Token abcdefg123456");

// Виконуємо запит і отримуємо код відповіді
int responseCode = con.getResponseCode();

// Показуємо повний URL запиту
System.out.println("Запит: " + url.toString());

// Показуємо отриманий код відповіді
System.out.println("Відповідь: " + responseCode);

// Закриваємо з'єднання
con.disconnect();
```

Після запуску коду ми отримаємо наступний вихід:

```
Запит: https://www.example.com/api/search?query=Java
Відповідь: 200
```

Ви можете змінювати метод запиту та додавати параметри за допомогою методів `setRequestMethod()` та `setRequestProperty()` відповідно. Детальніше про ці методи можна дізнатися з документації.

## Глибокі дослідження

Відправлення HTTP запиту полягає в установленні з'єднання з ресурсом за допомогою `HttpURLConnection` та використання різних методів цього класу для налаштування запиту. Після виконання запиту ми можемо отримати відповідь у вигляді коду статусу, вмісту сторінки або додаткових заголовків. Важливо правильно налаштувати запит, щоб отримати коректну відповідь.

## Дивіться також

- [Документація Java по `HttpURLConnection`](https://docs.oracle.com/javase/tutorial/networking/urls/