---
title:                "Виведення налагоджувальної інформації"
date:                  2024-01-20T17:52:14.771394-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Виведення інформації для налагодження допомагає бачити, що відбувається у вашій програмі. Програмісти користуються цим, щоб легше знаходити та виправляти помилки.

## Як це робити:
```Arduino
void setup() {
  // Початок серійної передачі зі швидкістю 9600 бодів на секунду
  Serial.begin(9600);
}

void loop() {
  // Виведення текcту через серійний порт
  Serial.println("Вітаємо у програмі!");

  // Затримка, щоб повідомлення не виводилось занадто часто
  delay(2000);
}
```
Sample Output:
```
Вітаємо у програмі!
Вітаємо у програмі!
...
```

## Поглиблений погляд:
Інформаційні виводи для налагодження - це стара добра звичка програмістів від часів, коли комп'ютеризація тільки зароджувалась. Альтернативи включають LED-індикацію, запис у файл або використання осцилографа для аналізу сигналу. Реалізація у Arduino проста завдяки вбудованому серійному порту, який дозволяє виводити текст прямо у консоль IDE або будь-який серійний монітор.

## Дивіться також:
- [Serial інструкції на офіційному сайті Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)