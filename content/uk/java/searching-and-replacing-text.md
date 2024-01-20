---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# "## Що і чому?"

Пошук та заміна тексту в програмуванні надзвичайно зручні для зміни текстових значень у програмі. Без них довелося б багато разів вручну перевіряти чи змінювати рядки, що займало б багато часу та зусиль.

## "## Як це зробити:"

Щоб шукати та замінювати текст на Java, ми можемо використовувати метод `replaceAll()` класу `String`. Подивимося на приклад:

```Java
public class Main {
    public static void main(String[] args) {
        String txt = "Привіт світе, Я в Java світі!";
        String newTxt = txt.replaceAll("світе", "дивосвіте");
        System.out.println(newTxt);
    }
}
```

Виведення:

```Java
Привіт дивосвіте, Я в Java дивосвіте!
```

Тут ми замінили всі входи слова "світе" на "дивосвіте".

## "## Глибше занурення:"

Пошук та заміна тексту - це стара програмна концепція, яка прийшла до нас зі старих мов програмування. Вона значно спрощує маніпуляції з текстом та підтримується більшістю сучасних мов.

Як альтернатива `replaceAll()`, ви можете використовувати `replace()` або `replaceFirst()`. Вибір зазвичай залежить від конкретного випадку використання.

Метод `replaceAll()` використовує регулярні вирази, тобто ви можете навіть виконувати більш складні заміни, використовуючи шаблони. 

## "## Дивись також:"

1. [Офіційна документація Java](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String))
2. [Посібник по регулярним виразам](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
3. [Туторіал по маніпуляціях з рядками в Java](https://www.baeldung.com/java-string-replace-tutorial)