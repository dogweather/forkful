---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Javascript: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і чому?
Перетворення рядка в нижній регістр - це процес заміни всіх великих букв в рядку на малі. Програмісти роблять це для полегшення порівняння рядків та пошуку даних, оскільки ця операція робить процес нечутливим до регістру.

## Як це робити:
Щоб перетворити рядок на нижній регістр в Arduino, ви можете використовувати наступний код:

```Arduino
String myString = "Hello, World!";
myString.toLowerCase();
Serial.println(myString);  // "hello, world!"
```

У цьому прикладі, ми використовуємо метод `toLowerCase()` із класу `String`, щоб перетворити `myString` на нижній регістр. Потім ми виводимо `myString` на Serial Monitor.

## Занурення в деталі
Перетворення рядка в нижній регістр - це часта задача в програмуванні, яка має важливе значення у процесах обробки тексту. Однак, важливо знати, що якщо ви працюєте з нон-ASCII символами, вам може бути потрібна більш складна логіка.

Інші альтернативи включають написання свого власного коду або використання зовнішніх бібліотек, які подолають стандартні обмеження.

У Arduino `toLowerCase()` працює шляхом ітерація по кожному символу рядка і зміни великих букв на малі, якщо вони наявні. Важливо розуміти, що цей метод змінює оригінальний рядок.

## Дивіться також
1. [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/) - Офіційна документація Arduino про об'єкти String
2. [Arduino String toLowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/) - Детальніше про `toLowerCase()`.
3. [Arduino пошук по рядку](https://www.arduino.cc/en/Tutorial/StringIndexOf) - Інший приклад використання рядків в Arduino, цього разу для пошуку підрядка в рядку.