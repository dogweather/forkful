---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Usuwanie znaków zgodnie z wzorcem to proces, który pozwala programiście zidentyfikować i usunąć konkretne znaki z ciągu (tekstu). Programiści robią to, aby oczyścić dane, usunąć zbędne znaki i upewnić się, że ciąg jest czytelny i zgodny z oczekiwanymi normami.

## Jak to zrobić:

Potrzebujesz prostego kodu Kotlin do usunięcia znaków z ciągu, które pasują do określonego wzorca? Oto on:

```Kotlin
fun String.usunZgodne(wzorzec: Regex): String {
    return this.replace(wzorzec, "")
}

fun main() {
    val tekst = "Cześć, jestem Kotek111 i lubię 222ryby333."
    println(tekst.usunZgodne(Regex("\\d")))  // usunie wszystkie cyfry
}
```

Na wyjściu otrzymasz:
```
Cześć, jestem Kotek i lubię ryby.
```

## Deep Dive:

1. **Kontekst historyczny**: Wymaganie usuwania znaków zgodnie z wzorcem istnieje od początku komputerów. Właśnie dlatego większość języków programowania, w tym Kotlin, ma wbudowane metody do wykonywania tej czynności.

2. **Alternatywy**: Można zastosować różne metody do usunięcia konkretnych znaków z ciągu, na przykład iterując przez ciąg znak po znaku czy używając funkcji `filterNot`. Wybór metody zależy od indywidualnych preferencji programisty i specyfiki problemu.

```Kotlin
fun String.usunZgodne(wzorzec: Char): String {
    return this.filterNot { it == wzorzec }
}
```

3. **Szczegóły implementacji**: Metoda `replace` używana w przykładzie działa poprzez zastępowanie znaków pasujących do określonego wzorca pustym ciągiem, co skutkuje usunięciem tych znaków.

## Zobacz też:

- [Dokumentacja Kotlin - Wyrażenia regularne](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/index.html)
- [Stack Overflow - Usuwanie znaków z ciągu Kotlin](https://stackoverflow.com/questions/52249280/removing-characters-from-string-with-kotlin)