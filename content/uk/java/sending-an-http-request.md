---
title:                "Надсилання запиту http"
html_title:           "Java: Надсилання запиту http"
simple_title:         "Надсилання запиту http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

Що і чому?
Надсилання HTTP запиту - це коли програміст надсилає запит до веб-сервера з метою отримання певних даних або виконання певної дії. Програмісти часто роблять це, щоб отримати доступ до даних з інших джерел або здійснити інтерактивні запити до веб-сайту.

Як це зробити:
Написання і відправлення HTTP запитів може бути досить простим за допомогою Java. Для цього вам знадобиться використати клас HttpURLConnection та методи для встановлення типу запиту (GET, POST, PUT, DELETE), встановлення заголовків та передачі даних. Нижче наведено приклад коду та вихідного виведення.

```Java
// Підключення до конкретної URL-адреси
URL url = new URL("https://example.com");

// Створення з'єднання та встановлення типу запиту
HttpURLConnection connection = (HttpURLConnection) url.openConnection();
connection.setRequestMethod("GET");

// Додавання заголовків до запиту (необов'язково)
connection.setRequestProperty("Content-Type", "application/json");
connection.setRequestProperty("User-Agent", "Java");

// Відправлення запиту та отримання відповіді
int responseCode = connection.getResponseCode();
BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
String inputLine;
StringBuilder response = new StringBuilder();
while ((inputLine = reader.readLine()) != null) {
	response.append(inputLine);
}

// Виведення відповіді на екран
System.out.println(response);
// Вихідне виведення: <html><body><h1>Hello World!</h1></body></html>
```

Глибше дослідження:
Надсилання HTTP запиту не новий процес, оскільки протокол HTTP існує з 1991 року. Однак, існує багато альтернативних методів для виконання подібних задач, таких як використання бібліотеки Apache HttpClient або використання більш високорівневих фреймворків, як Spring. При розробці програм з використанням HTTP запитів, необхідно уважно обробляти помилки та встановлювати з'єднання, щоб уникнути можливих проблем з безпекою.

Дивіться також:
Для більш детальної інформації про роботу з HTTP запитами в Java, перегляньте офіційну документацію Java та документацію класу HttpURLConnection. Також можна дізнатися більше про інші методи надсилання запитів, такі як POST та PUT.