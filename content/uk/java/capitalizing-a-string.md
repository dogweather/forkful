---
title:                "Запис строки з великої літери"
html_title:           "Java: Запис строки з великої літери"
simple_title:         "Запис строки з великої літери"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Що & Чому?

В програмуванні, просто кажучи, "капіталізація рядка" означає першу літеру кожного слова у рядку перетворити у велику літеру. Це один зі способів форматування рядків для полегшення читання. Програмісти часто капіталізують рядки для того, щоб покращити читабельність свого коду та зробити його більш зрозумілим для інших розробників.

Як?

```Java
public class CapitalizeString {
  public static void main(String[] args) {
    String name = "john doe";
    System.out.println("Before capitalization: " + name);
    name = name.toUpperCase();
    System.out.println("After capitalization: " + name);
  }
}

// Output:
// Before capitalization: john doe
// After capitalization: JOHN DOE
```

Глибоке пір'я

Історичний контекст:
Капіталізація рядків виникла з необхідності створення зрозумілого текстового формату для інформації, наприклад, в електронних таблицях та базах даних. У мовах програмування вона також застосовується для полегшення завантаження та швидкого знаходження інформації.

Альтернативи:
Є багато інших способів форматування рядків, таких як нижній регістр, CamelCase або Snake_case, й кожен з них має свої переваги в певних ситуаціях.

Деталі реалізації:
У Java є вбудований метод для капіталізації рядка - `toUpperCase()`, який можна викликати на об'єкті типу `String`. Цей метод повертає новий рядок з першою літерою кожного слова у великому регістрі.

Дивіться також:

- [Java Strings](https://www.w3schools.com/java/java_strings.asp)
- [String.toUpperCase() JavaDoc](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toUpperCase())