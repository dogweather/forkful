---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що це & Навіщо? 

Завантаження веб-сторінки - це процес отримання її даних через мережу. Програмісти роблять це, щоб отримати необхідну інформацію або взаємодіяти з веб-сервісами.

## Як це зробити:

Arduino дозволяє виконувати GET-запити для отримання веб-сторінок. Давайте згенеруємо код:

```Arduino
#include <Ethernet.h>

// Задайте MAC-адресу та IP-адресу для вашого контролера
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
IPAddress ip(192,168,1, 177);
EthernetClient client;

void setup() 
{
  Ethernet.begin(mac, ip);
  Serial.begin(9600);
}

void loop() 
{
  if (client.connected()) 
  {
    client.println("GET / HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
  } 
  else 
  {
    client.stop();
  }

  delay(5000);
}
```

## Поглиблений розбір:

Завантаження веб-сторінок - це універсальний інструмент, який може використовувати програміст. Історично, перше завантаження веб-сторінки відбулось у 1991 році. Можна використовувати POST або PUT методи, замість GET для відправки даних. Що стосується деталей реалізації, на Arduino ви опираєтесь на Ethernet або WiFi бібліотеки.

## Дивіться також:

- [Arduino Ethernet Library](https://www.arduino.cc/en/Reference/Ethernet)
- [HTTP протокол](https://www.w3.org/Protocols/rfc2616/rfc2616.html)
- [Arduino WiFi Library](http://arduino.cc/en/Reference/WiFi)