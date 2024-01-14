---
title:                "Kotlin: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

Існує багато ситуацій, коли програмістам потрібно замінити частини тексту у своєму коді. Це може бути необхідно для виправлення помилок, оновлення функціональності або покращення ефективності програми. Заміна тексту є важливою складовою частиною роботи з будь-якою мовою програмування, включаючи Kotlin.

## Як це зробити

Для заміни тексту в Kotlin можна використовувати різні методи. Один з найпростіших способів - це використовувати метод `replace()`:

```
val text = "Привіт, Світ!"
val newText = text.replace("Світ", "Котлін")
println(newText) // "Привіт, Котлін!"
```

Ви також можете використовувати регулярні вирази для більш гнучкого пошуку та заміни:

```
val text = "Кodíng еz Кotlin іs fun!"
val newText = text.replace(Regex("[еèé]"), "e")
println(newText) // "Coding ez Kotlin is fun!"
```

Заміна може також бути виконана за допомогою методів `replaceFirst()` та `replaceLast()`, які замінюють тільки перше або останнє співпадіння з шуканим текстом.

## Глибші дослідження

Якщо ви хочете більш детально дослідити можливості заміни тексту в Kotlin, рекомендую звернути увагу на додаткові методи та функції, такі як `replaceRange()`, `replaceBefore()`, `replaceAfter()`, `replaceAfterLast()`, `replaceBeforeLast()`. Вони дозволяють замінити текст після, до, перед або після останнього співпадіння з шуканим текстом.

## Дивіться також

- [Офіційна документація Kotlin з методами пошуку та заміни тексту](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Стаття про роботу з регулярними виразами у Kotlin](https://blog.mindorks.com/mastering-regex-in-kotlin)
- [Порівняння різних методів заміни тексту у Kotlin](https://www.jetbrains.com/help/idea/replacements-in-editor.html#5bf85187)

Бажаємо успіхів у вашому роботі з пошуком та заміною тексту у Kotlin!