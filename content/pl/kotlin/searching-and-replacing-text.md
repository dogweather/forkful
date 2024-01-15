---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Kotlin: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach edycja tekstu jest nieodłącznym elementem procesu tworzenia oprogramowania. Często musimy zmienić jedno lub więcej wyrażeń w naszym kodzie, aby poprawić błędy lub dostosować go do nowego wymagania. Dlatego też umiejętność wyszukiwania i wymiany tekstu jest niezbędna dla każdego programisty, aby efektywnie zarządzać swoim kodem.

## Jak to zrobić

Język Kotlin oferuje wiele przydatnych metod i funkcji, które ułatwiają wyszukiwanie i wymianę tekstu. Poniżej przedstawiam trzy sposoby, które możesz wykorzystać w swoim kodzie.

### Metoda replace()

Metoda replace() pozwala na prostą zamianę tekstu w danym ciągu znaków. W podanej składni, wystarczy podać dwa argumenty: wyrażenie lub ciąg znaków, które chcemy zastąpić oraz wyrażenie lub ciąg znaków którym chcemy je zastąpić.

```Kotlin
val text = "Witaj świecie"
val newText = text.replace("świecie", "kodzie")

println(newText)

// Output: Witaj kodzie
```

### Wyrażenie regularne

Jeśli chcemy przeprowadzić bardziej zaawansowaną zmianę tekstu, możemy skorzystać z wyrażenia regularnego. W Kotlinie mamy do dyspozycji funkcję replaceFirst(), która pozwala na wykonanie jednorazowej zamiany pierwszego pasującego do wzorca wyrażenia.

```Kotlin
val text = "1234 5678 9012"
val newText = text.replaceFirst(Regex("\\d{4}\\s"), "")

println(newText)

// Output: 5678 9012
```

### Funkcja replaceAll()

Aby dokonać wielokrotnej zamiany tekstu w jednym ciągu znaków, możemy skorzystać z funkcji replaceAll(). Ten sposób często jest konieczny podczas pracy z danymi tekstowymi.

```Kotlin
val text = "Kotlin, Java, Python, JavaScript"
val newText = text.replaceAll(Regex("[,\\s]"), "|")

println(newText)

// Output: Kotlin|Java|Python|JavaScript
```

## Głębsza analiza

Podczas pracy z tekstem, niektóre wyrażenia mogą być nieoczekiwane lub trudne do wykrycia. W takich przypadkach możliwe jest zastosowanie wyrażeń regularnych ze zmiennymi. Wyrażenia te zawierają symbol '$' przed numerem grupy, dzięki czemu możemy odwoływać się do już wykorzystanych fragmentów tekstu.

```Kotlin
val text = "XY-1234, ZA-5678, BC-9012"
val newText = text.replaceAll(Regex("([A-Z]{2})-(\\d{4})"), "$2-$1")

println(newText)

// Output: 1234-XY, 5678-ZA, 9012-BC
```

## Zobacz też

- [Dokumentacja języka Kotlin](https://kotlinlang.org/docs/reference/basic-types.html)
- [Oficjalna strona języka Kotlin](https://kotlinlang.org/)