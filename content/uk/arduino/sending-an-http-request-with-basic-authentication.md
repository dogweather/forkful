---
title:                "Надсилання http-запиту з основною автентифікацією"
html_title:           "Arduino: Надсилання http-запиту з основною автентифікацією"
simple_title:         "Надсилання http-запиту з основною автентифікацією"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Почему: Передаючи HTTP-запит з базовою аутентифікацією, ви можете забезпечити безпеку передаваних даних. Це важливо, якщо ви використовуєте вашу Arduino для зберігання чутливих даних або для комунікації з веб-сервером.

Як це зробити: Для початку, вам потрібно підключити вашу Arduino до Інтернету за допомогою Ethernet-шилда чи Wi-Fi модуля. Після цього, вам знадобиться налаштувати HTTP-запит і вказати параметри аутентифікації. Нижче подані приклади та результати для платформи Arduino UNO з Ethernet-шилдом.

```Arduino
// Підключення бібліотеки Ethernet
#include <Ethernet.h>

// Опір з'єднання
byte mac[] = {0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED};

// Адреса сервера з базовою аутентифікацією
char server[] = "your.server.com";

// Адреса HTTP-запиту та параметри аутентифікації
char path[] = "/api/data";
char auth[] = "username:password";

void setup() {
  // Ініціалізація Ethernet-контролера
  Ethernet.begin(mac);

  // Передача HTTP-запиту з авторизацією
  client.println("GET " + String(path) + " HTTP/1.1");
  client.println("Authorization: Basic " +base64.b64encode(String(auth)));
  client.println();

  // Отримання результатів
  while (client.available()) {
    char c = client.read();
    Serial.write(c);
  }
}

void loop() {

}
```

Результат виводу на моніторі портів виглядатиме приблизно так:

```
HTTP/1.1 200 OK
Date: Mon, 01 Nov 2021 00:00:00 GMT
Server: Apache
...
```

Розшифровування блоку коду:
- Підключаємо бібліотеку Ethernet та створюємо змінну для інтерфейсу з мак-адресою контролера.
- Вказуємо адресу сервера, який очікує базову аутентифікацію.
- Встановлюємо шлях до HTTP-запиту та параметри аутентифікації.
- Ініціалізуємо Ethernet-контролер та відправляємо HTTP-запит з логіном та паролем, закодованими у форматі [Base64](https://en.wikipedia.org/wiki/Base64).
- Очікуємо на результати та виводимо їх на монітор портів.

Глибоке занурення: Базова аутентифікація застосовує механізм передавання логіна та пароля у вигляді закодованого рядка у заголовку `Authorization` HTTP-запиту. При цьому, передавані дані не є криптографічно захищеними, тому не рекомендується використовувати їх для передачі чутливої інформації. К