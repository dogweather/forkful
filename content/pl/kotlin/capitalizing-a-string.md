---
title:                "Kotlin: Zmiana wielkości liter w ciągu znaków"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy nigdy zdarzyło Ci się chcieć zmienić wielkość liter w tekście, ale nie wiedzieć jak? Dzięki tej prostyemu krokowi w Kotlinie, będziesz miał możliwość szybko i łatwo zmienić wielkość liter w swoim tekście. Nie musisz już się martwić, o tytuły w różnych formatach, bo teraz będziesz mógł szybko je zmienić dzięki niewielkiej funkcji w Kotlinie.

## Jak to zrobić

Aby zmienić wielkość liter w tekście w Kotlinie, wystarczy użyć funkcji ```toUpperCase()``` lub ```toLowerCase()```. Oto przykładowy kod:

```Kotlin 
val originalText = "To jest przykładowy tekst."
val capitalizedText = originalText.toUpperCase()
```

Wynikiem powyższego kodu będzie zmodyfikowany tekst zapisany w zmiennej ```capitalizedText```, który wyglądał będzie następująco: "TO JEST PRZYKŁADOWY TEKST." W przypadku użycia funkcji ```toLowerCase()``` wynik byłby taki: "to jest przykładowy tekst." 

## Głębsze zagadnienia

Jeśli chcesz dokładniej poznać działanie funkcji ```toUpperCase()``` i ```toLowerCase()```, warto wiedzieć, że działają one na podstawie znaków Unicode. Oznacza to, że jeśli w Twoim tekście zawarte są znaki diakrytyczne, zostaną one odpowiednio zmienione z zachowaniem wielkości liter. 

Na przykład, słowo "żółty" po użyciu funkcji ```toUpperCase()``` będzie wyglądało tak: "ŻÓŁTY", a po użyciu funkcji ```toLowerCase()``` będzie wyglądało tak: "żółty". Dzięki temu możesz mieć pewność, że nie ma znaczenia, czy w tekście znajdują się polskie, czy angielskie słowa, funkcja zadziała poprawnie.

## Zobacz także

- Biblioteka funkcji standardowych w Kotlinie (https://kotlinlang.org/docs/reference/stdlib.html)
- Podstawy Kotlin dla początkujących (https://kotlinlang.org/docs/reference/basic-syntax.html)