---
title:                "Java: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# Чому

Забирання підрядків є важливою частиною розробки програмного забезпечення, оскільки дозволяє отримувати певні частини тексту з рядка. Це допомагає програмістам маніпулювати даними та виконувати різні дії з ними, що робить процес програмування більш ефективним та швидким.

# Як це зробити

Існує кілька способів виділення підрядків в Java. Одним з найпростіших способів є використання методу `substring()`. У цьому прикладі ми заберемо підрядок з рядка "Hello world" використовуючи цей метод:

```Java
String str = "Hello world";
String substr = str.substring(6);
System.out.println(substr); // виведе "world"
```

Ми також можемо вказати початкову та кінцеву позиції підрядка, наприклад:

```Java
String str = "Hello world";
String substr = str.substring(3, 7);
System.out.println(substr); // виведе "lo w"
```

# Глибша аналітика

Метод `substring()` використовується для виділення підрядків з рядка шляхом вказання позицій початку та кінця цього підрядка. Якщо не вказувати кінцеву позицію, метод поверне підрядок до кінця рядка. Також можна вказати від'ємні значення позицій, що означає рахувати з кінця рядка. Наприклад, `-5` означає позицію п'ять символів до кінця рядка.

Іншим способом виділення підрядків є використання методу `split()`, який розділяє рядок на підрядки за допомогою певного роздільника. Наприклад:

```Java
String str = "Hello world";
String[] substrings = str.split(" ");
System.out.println(substrings[0]); // виведе "Hello"
System.out.println(substrings[1]); // виведе "world"
```

# Дивись також

- [Документація по методу substring()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [Документація по методу split()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#split-java.lang.String-)