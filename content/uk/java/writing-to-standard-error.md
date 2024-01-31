---
title:                "Запис в стандартний потік помилок"
date:                  2024-01-19
simple_title:         "Запис в стандартний потік помилок"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Стандартна помилка (stderr) - це потік для виводу помилок та діагностики. Користуються нею, щоб відокремити помилки від звичайного виводу програми.

## How to:
```java
public class StdErrExample {
    public static void main(String[] args) {
        System.out.println("Звичайний вивід");
        System.err.println("Повідомлення про помилку");
    }
}
```
Вивід:
```
Звичайний вивід
Повідомлення про помилку
```

## Deep Dive
Стандартна помилка, на відміну від стандартного виводу (stdout), з'явилася, щоб розділити 'звичайні' дані та повідомлення про помилки. Альтернативою є перенаправлення stderr у файл або інші потоки. У Java, `System.err` використовують, як і `System.out`, тільки воно за замовчуванням червоного кольору в консолі, щоб привернути увагу.

## See Also
- [Oracle - System Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/System.html)
- [Wikipedia - Standard Streams](https://en.wikipedia.org/wiki/Standard_streams)
