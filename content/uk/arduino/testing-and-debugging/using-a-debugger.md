---
date: 2024-01-26 03:48:20.495661-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 Arduino IDE \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0432\
  \u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u0447\u0435\u0440\u0435\u0437\
  \ Serial \u0434\u043B\u044F \u0434\u0435\u0431\u0430\u0433\u0456\u043D\u0433\u0443\
  , \u0430\u043B\u0435 \u0446\u0435 \u0442\u0440\u043E\u0445\u0438 \u0441\u0445\u043E\
  \u0436\u0435 \u043D\u0430 \u0434\u043E\u0441\u043B\u0456\u0434\u0436\u0435\u043D\
  \u043D\u044F \u043F\u0435\u0447\u0435\u0440\u0438 \u0437\u0430 \u0434\u043E\u043F\
  \u043E\u043C\u043E\u0433\u043E\u044E\u2026"
lastmod: '2024-03-13T22:44:49.725712-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Arduino IDE \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438\
  \ \u0432\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u0447\u0435\u0440\u0435\
  \u0437 Serial \u0434\u043B\u044F \u0434\u0435\u0431\u0430\u0433\u0456\u043D\u0433\
  \u0443, \u0430\u043B\u0435 \u0446\u0435 \u0442\u0440\u043E\u0445\u0438 \u0441\u0445\
  \u043E\u0436\u0435 \u043D\u0430 \u0434\u043E\u0441\u043B\u0456\u0434\u0436\u0435\
  \u043D\u043D\u044F \u043F\u0435\u0447\u0435\u0440\u0438 \u0437\u0430 \u0434\u043E\
  \u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043B\u0456\u0445\u0442\u0430\u0440\u0438\
  \u043A\u0430."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0434\
  \u0435\u0431\u0430\u0433\u0435\u0440\u0430"
weight: 35
---

## Як це зробити:
У Arduino IDE ви можете використовувати виведення через Serial для дебагінгу, але це трохи схоже на дослідження печери за допомогою ліхтарика. Для справжнього дебагінгу вам може знадобитися крок вперед з чимось на кшталт дебагера Atmel-ICE, який інтегрується з середовищем Arduino. Ось приклад псевдо-дебагінгу через Serial:

```Arduino
void setup() {
  Serial.begin(9600);
}
void loop() {
  int sensorValue = analogRead(A0);
  Serial.print("Значення датчика: ");
  Serial.println(sensorValue);
  // Уявіть, що ви очікуєте отримати 512, але отримуєте 0.
  // Час перевірити з'єднання датчика
  delay(1000); // Зачекати секунду перед повторним читанням
}
```
Запустіть це з відкритим монітором Serial, і ви побачите, що ваш датчик видає в реальному часі.

## Поглиблений розгляд
До появи дебагерів світ належав операторам виводу - ви могли лише здогадуватися, що відбувається, виводячи все на екран. Дебагінг за допомогою виводу все ще поширений, особливо в простіших середовищах або на обмеженому апаратному забезпеченні, як-от Arduino.

Альтернативи інтерфейсним емуляторам на кшталт Atmel-ICE включають програмні інструменти для дебагінгу, наприклад, `avr-gdb`. Ви можете поєднати його з `avarice`, аби створити міст між GDB і вашим апаратним забезпеченням, що є дуже зручним для більш розширеного дебагінгу безпосередньо на чіпі.

Використовуючи дебагер, ви можете встановлювати точки зупинки, щоб призупинити виконання в певних місцях. Ви можете крок за кроком пройти через свій код, переглянути пам'ять, регістри та змінні. Це дозволяє вам виявляти проблеми, а не стріляти в темряві. Коли ви впроваджуєте дебагер, переконайтеся, що ваше середовище налаштоване правильно - несумісні версії або погано налаштовані інструменти можуть призвести до фрустрації.

## Дивіться також
Готові піти глибше? Зануртесь у наступне:
- Посібник з дебагінгу Arduino на [Arduino Debugging](https://www.arduino.cc/en/Guide/Environment#toc7)
- Референсний мануал AVR Libc для налаштування avr-gdb: [Домашня сторінка AVR Libc](http://www.nongnu.org/avr-libc/)
