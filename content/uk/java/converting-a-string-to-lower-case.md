---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Elixir: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і чому?

Перетворення рядка на нижній регістр - це процес заміни всіх символів верхнього регістра на відповідні символи нижнього регістра. Програмісти це роблять, щоб стандартизувати і спростити обробку текстових даних.

## Як це робиться:

```Java
public class Main {
     public static void main(String[] args) {
         String str = "HELLO, WORLD!";
         String lowerCaseStr = str.toLowerCase();
         System.out.println(lowerCaseStr);
     }
}
```

Цей код представить такий результат:

```Java
"hello, world!"
```

## Поглиблений розбір:

### Історичний контекст:
Із появою комп'ютерів виникла потреба у стандартизації текстових даних, однією з яких є перетворення всіх символів на нижній регістр.

### Альтернативи:
В Java є метод `Character.toLowerCase()`, який перетворює окремий символ у нижній регістр.

### Деталі реалізації:
У Java метод `toLowerCase()` використовує правила локального перетворення символів для переведення символів у нижній регістр.

## Додатково:

Для більш детального ознайомлення з роботою з рядками в Java, відвідайте офіційну документацію Oracle [тут](https://docs.oracle.com/javase/tutorial/java/data/strings.html).