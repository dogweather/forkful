---
title:                "Запис в стандартний потік помилок"
html_title:           "Arduino: Запис в стандартний потік помилок"
simple_title:         "Запис в стандартний потік помилок"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Що і чому?

Стандартний потік помилок (stderr) - це канал, яким програми передають повідомлення про помилки і діагностику. Програмісти використовують його, щоб відокремити звичайний вивід програми від повідомлень про помилки, що дозволяє ефективно ловити і обробляти помилки.

## Як це робити:

В Arduino, стандартний потік помилок відсутній так, як в Unix-подібних системах, але можна емулювати його через Serial. Ось як:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  Serial.print("Просте повідомлення\n");
  Serial.flush();  // Переконаймося, що все з буфера було відправлене

  // Імітація stderr
  Serial.print("Помилка: Щось пішло не так\n");
  Serial.flush();
}
```

## Поглиблено:

В Unix-подібних системах, stderr - це один з трьох основних потоків даних, запроваджених кілька десятиліть тому. У Arduino немає розділення на stdout і stderr, але через Serial можна симулювати подібну поведінку, як показано вище. Важливо розуміти, що Serial просто передає дані через USB у комп'ютер, і їх подальше використання визначається вже на боці ПК чи ноутбука.

## Дивіться також:

- [Serial library reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Управління потоками даних у C++](https://en.cppreference.com/w/cpp/io)
- [Історія Unix-подібних систем](https://www.levenez.com/unix/)
