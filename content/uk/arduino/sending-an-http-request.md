---
title:                "Надсилання запиту http"
html_title:           "Arduino: Надсилання запиту http"
simple_title:         "Надсилання запиту http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і для чого?
Відправка HTTP-запиту - це процес надсилання запиту на веб-сервер за допомогою протоколу HTTP. Програмісти цього делають, щоб отримати доступ до різноманітної інформації з Інтернету, від веб-серверів, додатків та інших джерел.

## Як це зробити:
У наступних блоках коду ```Arduino ... ```, ви можете побачити приклади програм, що демонструють відправку HTTP-запиту і вивід результату на відладочний порт.

Наприклад, якщо ви хочете перевірити погоду у вашому місті за допомогою API OpenWeatherMap, можна використати такий код:

```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

void setup() {
  Serial.begin(9600);
  WiFi.begin("назва мережі Wi-Fi", "пароль мережі");
  
  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Встановлюється підключення до мережі Wi-Fi...");
  }
  
  HTTPClient http;
  String url = "http://api.openweathermap.org/data/2.5/weather?q=Київ,UA&APPID=API-ключ";
  http.begin(url);
  int httpCode = http.GET();
  
  if (httpCode > 0) {
    Serial.printf("[HTTP] GET повернув код: %d\n", httpCode);
    
    if (httpCode == HTTP_CODE_OK) {
      String payload = http.getString();
      Serial.println(payload);
    }
  } else {
    Serial.printf("[HTTP] Помилка підключення: %s\n", http.errorToString(http.errorCode()).c_str());
  }
  
  http.end();
}

void loop() {
  
}
```

Результатом програми буде виведення даних про погоду у Києві на відладочний порт.

## Глибоке занурення:
Відправка HTTP-запитів стала надзвичайно популярною з появою Інтернету. Це стало можливим завдяки протоколу HTTP, що дозволяє обмінюватися інформацією між веб-серверами та програмами. Є інші альтернативи відправки запитів, наприклад, використання FTP або TCP-з'єднання, але HTTP-протокол широко використовується через його простоту та відкритий стандарт.

Для виконання HTTP-запиту, вам необхідно мати діюче підключення до Інтернету та знати URL-адресу, на яку ви хочете надіслати запит. Також може бути необхідно зазначити параметри запиту, а також опційний заголовок або тіло запиту.

## Дивіться також:
- Документація Arduino: https://www.arduino.cc/en/Reference/HTTPClient
- Офіційний сайт OpenWeatherMap API: https://openweathermap.org/api
- Приклади використання HTTP-запитів на Arduino: https://randomnerdtutorials.com/esp32-http-get-post-arduino/