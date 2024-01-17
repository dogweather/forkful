---
title:                "Використання регулярних виразів"
html_title:           "Arduino: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що & Чому?
Регулярні вирази - це спеціальні шаблони, які допомагають програмістам шукати та опрацьовувати тексти згідно з заданими критеріями. Вони є потужним інструментом для роботи з текстовими даними і дозволяють ефективно здійснювати операції, які раніше вимагали багато коду.

## Як:
```Arduino
// Приклад використання регулярних виразів для перевірки правильності електронної адреси
String email = "test@example.com";

if (email.match(/^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}$/i)) {
  Serial.println("Електронна адреса вірна");
} else {
  Serial.println("Невірна електронна адреса");
}

// Приклад використання регулярних виразів для перетворення тексту вищого регістра в нижній
String text = "HELLO, WORLD";

text.toLowerCase();

Serial.println(text); // виведе "hello, world"
```

## Глибокий занурений:
Регулярні вирази були створені в 1956 році і використовувалися в багатьох мовах програмування, зокрема у Perl, Java та JavaScript. Є альтернативи до регулярних виразів, такі як використання вбудованих методів рядків, але вони можуть бути менш ефективними та потребувати більше коду. Для використання регулярних виразів у середовищі Arduino необхідно використовувати бібліотеку regex.

## Дивіться також:
- [Документація по бібліотеці regex для Arduino](https://www.arduino.cc/reference/en/libraries/regex/)
- [Основи регулярних виразів](https://www.regular-expressions.info/tutorial.html)
- [10 корисних прикладів використання регулярних виразів](https://www.keycdn.com/blog/regular-expressions-examples)