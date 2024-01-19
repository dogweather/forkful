---
title:                "Konkatenacja ciągów znaków"
html_title:           "Bash: Konkatenacja ciągów znaków"
simple_title:         "Konkatenacja ciągów znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konkatenacja ciągów polega na łączeniu dwóch lub więcej ciągów znaków w jeden. Programiści korzystają z tego, aby manipulować danymi tekstowymi i utworzyć dynamiczne ciągi znaków.

## Jak to zrobić:

Wykorzystaj operator `+` do łączenia ciągów. Oto przykładowy kod:

```Kotlin
fun main() {
    val str1 = "Cześć "
    val str2 = "Świecie!"
    val result = str1 + str2
    println(result) // Wyświetli: "Cześć Świecie!"
}
```

Możesz też użyć funkcji `plus`.

```Kotlin
fun main() {
    val str1 = "Cześć "
    val str2 = "Świecie!"
    val result = str1.plus(str2)
    println(result) // Wyświetli: "Cześć Świecie!"
}
```

## Dogłębna analiza

Concatenacja była obecna w wielu starszych językach takich jak C czy Java, stąd pojawienie się w Kotlinie. Alternatywą dla niej może być interpolacja ciągów (czyli wstawianie odpowiednich wartości do ciągów).

Chociaż funkcje plus i operator `+` działają podobnie, musisz pamiętać, że obie tworzą nowy łańcuch, który jest połączeniem obu łańcuchów. Nie zmieniają one oryginalnych łańcuchów.

## Zobacz też

Do bardziej skomplikowanych operacji związanych z ciągami, poznaj Bibliotekę Standardową Kotlin dla Stringów.
([Tutaj](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/))

Interpolacja ciągów może być bardziej wydajna, jeśli konkatenujesz wiele ciągów jednocześnie. Więcej na ten temat znajdziesz [tutaj](https://kotlinlang.org/docs/basic-types.html#string-templates).