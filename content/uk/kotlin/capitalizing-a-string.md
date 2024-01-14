---
title:    "Kotlin: Капіталізація рядка"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Для чого

Уперше у програмуванні може здатися, що перетворення тексту на великі літери - це пересічна задача, і навіщо взагалі ним займатися? Однак, цілком логічним є бажання, наприклад, виводити користувачу назви програм або веб-сторінки у вигляді заголовків з великих літер. Не забуваємо про чистоту коду, і в цьому випадку правильне форматування тексту також важливе!

## Як те зробити

Потрібно лише використати вбудовану функцію `capitalize()`, яку надає мова Kotlin. Нижче наведено приклад коду, який перетворює рядок `hello world` на `Hello world`:

```Kotlin
val text = "hello world"
println(text.capitalize())
```

Результатом буде виведення `Hello world`. Якщо вам потрібно перетворити на великі літери кожне слово в рядку, використовуйте функцію `capitalizeWords()`, яка розділює рядок на окремі слова та перетворює їх:

```Kotlin
val text = "hello world"
println(text.capitalizeWords())
```

Результатом буде виведення `Hello World`.

## Глибока розкриття

Функції `capitalize()` та `capitalizeWords()` базуються на стандартній бібліотеці мови Kotlin та використовують метод `toUpperCase()` для перетворення рядків великі літери. Якщо ви хочете самостійно реалізувати подібний функціонал, можете скористатися цим методом і додати додаткові дії зі зміною форматування рядка.

## Дивіться також

- [Документація Kotlin про `capitalize()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Документація Kotlin про `capitalizeWords()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize-words.html)
- [Документація Kotlin про `toUpperCase()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html)