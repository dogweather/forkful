---
title:                "Arduino: З'єднання рядків."
simple_title:         "З'єднання рядків."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# Для чого

Конкатенація рядків - це важливий елемент програмування Arduino, який дозволяє об'єднувати різні рядки для створення нових повідомлень або даних. Це особливо корисно для створення текстових повідомлень для відображення на LCD екрані або для відправлення через серійний порт.

# Як

```arduino
String firstName = "Андрій";
String lastName = "Петренко";
String fullName = firstName + " " + lastName;
Serial.println(fullName);
```

Вивід: "Андрій Петренко"

Цей приклад показує, як за допомогою оператора "+" можна об'єднати різні рядки в один. Також можна використовувати цей метод для об'єднання чисел і рядків.

Існують інші методи конкатенації рядків, такі як ```concat()```, ```sprintf()``` та ```strcat()```, але використання "+" є більш простим і зрозумілим для початківців.

# Глибоке занурення

В Arduino, рядки є об'єктами класу ```String```, тому для конкатенації потрібно використовувати методи цього класу, такі як "+" або ```concat()```. При використанні цих методів потрібно враховувати обмеження пам'яті мікроконтролера, оскільки багато рядків може призвести до переповнення пам'яті і викликати помилки.

Також, при створенні рядків з великою кількістю даних, важливо використовувати оптимальний тип даних ```char``` замість ```String```, щоб уникнути витрачання пам'яті.

# Дивіться також

- [Arduino документація про рядки](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Стаття про використання рядків в Arduino](https://maker.pro/arduino/tutorial/how-to-use-strings-in-arduino-programming)
- [Відео урок з основами роботи з рядками в Arduino](https://www.youtube.com/watch?v=ld4H8afyK_Q&ab_channel=TronixExplorer)