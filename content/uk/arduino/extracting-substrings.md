---
title:                "Видобування підрядків"
html_title:           "Arduino: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# Що i для чого ?

Видiлення пiдstrilings - це процес видiлення частини рядка, який ми можемо використовувати окремо вiд вихiдного рядка. Програмiсти використовують цей процес для зручностi та економiї часу, роздiляючи великi рядки на меншi частини для подальшої обробки чи використання.

# Як це зробити:

```
Arduino ... ``` код блоки
```

Щоб видiлити пiдстрока, ми використовуємо функцiю substring () в Arduino. Наприклад:

```
String str = "Привiт, свiте!";
String sub = str.substring(7); // пiдстрока буде: "свiте!"
```

Ви також можете вказати початковий та кiнцевий iндекс для видiлення бiльш точної пiдстроки, як показано в наступному прикладi:

```
String date = "19/05/2021";
String day = date.substring(0,2); // пiдстрока буде: "19"
String month = date.substring(3,5); // пiдстрока буде: "05"
String year = date.substring(6); // пiдстрока буде: "2021"
```

Ви також можете використовувати цю функцiю для роздiлення рядка на масив, як показано в наступному прикладi:

```
String animals = "кiт, собака, кiнь, птах";
String arr[4];
for(int i = 0; i < 4; i++) {
  arr[i] = animals.substring(0, animals.indexOf(","));
  animals = animals.substring(animals.indexOf(",") + 2);
}
// arr[0] буде: "кiт"
// arr[1] буде: "собака"
// arr[2] буде: "кiнь"
// arr[3] буде: "птах"
```

# Розглянути питання детальнiше:

У видiленнi пiдстроки у Arduino є довга iсторiя та багато альтернатив. Ви можете також використовувати функцiю charAt () для отримання окремого символу з рядка. Це може бути корисно для рядкiв, що мiстять лише цифри, як слова або дата, де ви можете просто звертатися до певного символу за його позицiєю.

Також, важливо знати, що при використаннi функцiї substring () у String рядках, Arduino створює новий рядок, що може призвести до витрат пам'ятi, особливо якщо роботаєте з великими рядками. Для економiї пам'ятi, ви можете використовувати функцiю c_str (), що повертає звичайний масив символiв, а не рядок String.

# Посилання:

- [Arduino документацiя про функцiю substring ()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Просте рiшення для потьючногороботи з рядками в Arduino](https://www.arduinenthusiast.com/strcat-in-arduino/)
- [Стаття про ефективне використання пам'ятi у Arduino](https://www.itead.cc/blog/efficient-memory-usage-trick-in-arduino)