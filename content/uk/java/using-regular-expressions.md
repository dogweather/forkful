---
title:                "Java: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Чому

Регулярні вирази є потужним інструментом для обробки і аналізу тексту в програмі на Java. Вони дозволяють швидко і ефективно шукати, заміняти і виокремлювати певні шаблони у текстових рядках. Якщо ви працюєте з текстовими даними, використання регулярних виразів може значно спростити вашу роботу.

## Як

Для початку, вам потрібно імпортувати клас `java.util.regex.Pattern` та `java.util.regex.Matcher`. Потім використовуйте метод `compile()` класу `Pattern`, щоб створити об'єкт типу `Pattern` і передати йому регулярний вираз у вигляді рядка. Наступним кроком є створення об'єкту типу `Matcher`, використовуючи метод `matcher()` класу `Pattern` і передавши йому текст, який ви хочете перевірити. Нарешті, використовуйте методи `find()` і `matches()` для пошуку відповідності регулярному виразу у тексті.

```Java
Pattern pattern = Pattern.compile("Java");
Matcher matcher = pattern.matcher("Hello, Java!");
if (matcher.find()) {
    System.out.println("Знайдено відповідність!");
}
```

Ви також можете використовувати регулярні вирази для заміни частин тексту. Для цього використовуйте метод `replaceAll()` з об'єктом типу `Matcher` та передайте йому рядок, який потрібно замінити та новий рядок, на який потрібно замінити.

```Java
Pattern pattern = Pattern.compile("Java");
Matcher matcher = pattern.matcher("Hello, Java!");
String newString = matcher.replaceAll("World");
System.out.println(newString);
```

Вихідним рядком буде "Hello, World!".

## Глибша інформація

Особливу увагу слід звернути на особливості синтаксису регулярних виразів для Java. Наприклад, для означення цілочисельних та десяткових чисел використовуються `\d` та `\D` відповідно. Також важливо пам'ятати про спеціальні символи, такі як `.`, який означає будь-який символ, та `+`, який означає одне або більше повторень попереднього символу або групи символів.

## Дивись також

- [Офіційна документація Java для регулярних виразів](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Ручниці по регулярних виразах в Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [10 простих прикладів використання регулярних виразів в Java](https://www.javacodegeeks.com/2012/02/10-examples-of-using-regular.html)