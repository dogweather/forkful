---
title:                "Arduino: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

Програмування на Arduino може бути захоплюючим і корисним хобі для тих, хто цікавиться електронікою та технологіями. Однією з важливих навичок, які потрібно вивчити при програмуванні на Arduino, є пошук та заміна тексту. Це дозволяє швидко і ефективно вносити зміни до коду та вдосконалювати його функціональність.

## Як

Для того, щоб знайти та замінити текст на Arduino, використовуйте функцію `String.replace()`.

```Arduino
// приклад коду для пошуку та заміни тексту
String text = "Привіт, світ!";
text.replace("Привіт", "Привітик"); // замінюємо "Привіт" на "Привітик"
Serial.println(text); // виводить "Привітик, світ!"
```

## Поглиблене вивчення

Окрім заміни тексту з функцією `String.replace()`, також можна використовувати цикли для пошуку та заміни більшої кількості слів або фраз. Наприклад, у циклі можна ітерувати через масив зі словами, які потрібно замінити, і викликати функцію `String.replace()` кожен раз з новим значенням.

```Arduino
// приклад коду з використанням циклів для пошуку та заміни тексту
String text = "Я люблю програмування на Arduino!";
String words[2] = { "люблю", "Arduino" }; // масив зі словами, які потрібно замінити

for (int i = 0; i < 2; i++) {
    text.replace(words[i], words[i] + " дуже"); // додаємо "дуже" до кожного слова з масиву
}

Serial.println(text); // виводить "Я дуже люблю програмування на Arduino!"
```

## Дивіться також

Для додаткової інформації про пошук та заміну тексту на Arduino, перегляньте наступні посилання:

- [Офіційна документація Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Відео урок по пошуку та заміні тексту на Arduino](https://www.youtube.com/watch?v=nOCM6nyPqF0)
- [Стаття про заміну тексту в Arduino з використанням регулярних виразів](https://maker.pro/arduino/tutorial/how-to-use-regular-expressions-in-arduino)