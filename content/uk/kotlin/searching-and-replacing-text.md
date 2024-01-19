---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і чому?
Пошук та заміна тексту це процес виявлення певного фрагмента коду та його заміни на інший. Програмісти роблять це, щоб виправити помилки, внести зміни або оптимізувати код.

## Як це робити:
Kotlin забезпечує методи `replace(oldValue, newValue)` та `replaceFirst(oldValue, newValue)` для цієї мети. Це ось як вони виглядають на практиці:

```Kotlin
fun main() {
    var str = "Hello, Kotlin!"
    println(str.replace("Kotlin", "World"))
   str = "Kotlin, Kotlin!"
    println(str.replaceFirst("Kotlin", "World"))
}
```

Цей код спочатку виведе "Hello, World!", а потім "World, Kotlin!", замінивши лише перше входження слова "Kotlin".

## Докладніше
Заміна тексту скоріш за все бере своє поділ на старих механічних письмових машинах, де інженери обхідно заміняти рулони, щоб змінити текст. В середовищі програмування ми маємо багато альтернатив, таких як регулярні вирази для складніших сценаріїв заміни. Що стосується виконання, Kotlin перебирає весь текст, шукаючи входження старого значення, і замінює його при знаходженні.

## Додаткова інформація
- [Документація Kotlin по функції replace](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Детальніше про регулярні вирази в Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Стандартна бібліотека Kotlin для обробки тексту](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/)