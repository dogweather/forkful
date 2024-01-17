---
title:                "Використання регулярних виразів"
html_title:           "Java: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

Що і чому?
Використання регулярних виразів у програмуванні - це процес пошуку збігів у тексті за допомогою спеціальних шаблонів. Це дозволяє програмістам більш ефективно обробляти та аналізувати великі обсяги даних.

Як?
Перш ніж почати використовувати регулярні вирази в Java, необхідно імпортувати пакет java.util.regex. Нижче наведені приклади коду, які демонструють основи використання регулярних виразів у Java:

```Java
// Перевірка чи відповідає рядок певному шаблону
String pattern = "a*b";
String input = "aaab";
boolean isMatch = Pattern.matches(pattern, input);
System.out.println(isMatch); // вивід: true
```

```Java
// Заміна підрядка у рядку за допомогою регулярного виразу
String pattern = "a*b";
String input = "Hello aaaaab World";
input = input.replaceAll(pattern, "X");
System.out.println(input); // вивід: Hello X World
```

```Java
// Вибірка смайликів з тексту за допомогою регулярного виразу
String pattern = "(:|;)(-|~)?(\\)|D|P)";
String input = "Hello :) World :D";
Pattern smileyPattern = Pattern.compile(pattern);
Matcher matcher = smileyPattern.matcher(input);
while (matcher.find()) {
    System.out.print(matcher.group() + " "); // вивід: :) :D
}
```

Поглиблене вивчення
Використання регулярних виразів доволі поширена практика у програмуванні. Історично, ця технологія була вперше використана в Unix-системах для обробки текстових файлів. Однак, на сьогоднішній день існують інші альтернативи, такі як функції заміни та пошуку у рядках, але регулярні вирази залишаються більш гнучким та потужним інструментом.

Для запам'ятовування регулярних виразів у Java використовується синтаксис, що базується на Perl, де кожен символ у шаблоні відповідає певній комплексній послідовності символів. Детальніше про синтаксис можна почитати у документації Java.

Дивитися також
- [Документація Java про регулярні вирази](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Інтерактивний гайд про використання регулярних виразів у Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)