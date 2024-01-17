---
title:                "Пошук та заміна тексту"
html_title:           "Arduino: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що & Чому?

У програмуванні часто потрібно шукати та замінювати певний текст. Наприклад, якщо ви хочете замінити всі входження одного слова на інше в своєму коді. Програмісти роблять це для полегшення редагування та покращення ефективності свого коду.

## Як:

Простий спосіб замінити текст в Arduino полягає у використанні функції ```replace()```. Нижче наведений приклад коду, який замінює усі входження слова "привіт" на "добрий день" у рядку ```message```:

```
Arduino

String message = "Привіт, світ!";
message.replace("привіт", "добрий день");
Serial.println(message); // виведе "Добрий день, світ!"
```

У цьому прикладі функція ```replace()``` замінює усі входження слова "привіт" на "добрий день" у змінній ```message```. Використовуючи цю функцію, ви можете змінити будь-який текст за необхідним шаблоном.

## Глибоке дослідження:

Пошук та заміна тексту не є новими поняттями. Давнішими версіями Arduino могло бути важче виконати подібну задачу, але з випуском Arduino 1.0 це стало набагато простіше завдяки функції ```replace()```. Також існують альтернативні способи заміни тексту, наприклад, можна використовувати регулярні вирази та інші методи функцій рядків.

## Дивіться також:

- [DZone: Пошук та заміна тексту в Arduino] (https://dzone.com/articles/dealing-with-strings-in-arduino)
- [Arduino Reference: String Function ```replace()```] (https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Tutorialspoint: Робота з рядками в Arduino] (https://www.tutorialspoint.com/arduino/arduino_strings.htm)

Будь ласка, залишайте коментарі та поділіться своїми власними способами пошуку та заміни тексту в Arduino!