---
title:                "Java: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# З чого почати: Капіталізація рядкових даних

Капіталізація рядкових даних - це процес перетворення першої літери кожного слова в рядку на велику. Це може бути корисним при форматуванні тексту або при відображенні імен користувачів в програмі. Давайте поглянемо на те, як реалізувати це в Java.

## Як це зробити

Для початку створимо змінну типу String та присвоїмо їй будь-який текст, який ми хочемо капіталізувати.

```Java
String text = "цей текст буде капіталізований";
```

Тепер нам потрібно розділити рядок на окремі слова. Це можна зробити за допомогою методу `split()`, який розділяє текст на масив рядкових значень за заданим роздільником. У нашому випадку роздільником буде пробіл.

```Java
String[] words = text.split(" ");
```

Далі ми можемо перебрати отриманий масив слів та використовувати методи класу `String` для капіталізації першої літери кожного слова. Наприклад, метод `substring()` дозволяє нам отримати певну частину рядку, а метод `toUpperCase()` перетворює символи на великі літери.

```Java
for (int i = 0; i < words.length; i++) {
  words[i] = words[i].substring(0, 1).toUpperCase() + words[i].substring(1).toLowerCase();
}
```

Нарешті, склеємо наші слова знову в один рядок за допомогою методу `join()`, який приймає роздільник та масив значень.

```Java
String capitalizedText = String.join(" ", words);
```

Весь наш код можна зібрати в один метод та застосувати до будь-якого рядка, який хочете капіталізувати.

```Java
public static String capitalizeString(String text) {
  String[] words = text.split(" ");
  for (int i = 0; i < words.length; i++) {
    words[i] = words[i].substring(0, 1).toUpperCase() + words[i].substring(1).toLowerCase();
  }
  return String.join(" ", words);
}


// Використання
String originalText = "цей текст буде капіталізований";
String capitalizedText = capitalizeString(originalText);
System.out.println(originalText); // цей текст буде капіталізований
System.out.println(capitalizedText); // Цей Текст Буде Капіталізований
```

## Детальний огляд

У вище наведеному прикладі ми використовували метод `split()` з роздільником " " для розбиття рядка на окремі слова. Але, якщо ви хочете розбити рядок за допомогою іншого роздільника, ви можете передати його як аргумент у метод `split()`. Наприклад, якщо ви хочете розбити слова, розділені ком