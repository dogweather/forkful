---
title:                "Javascript: Zapisywanie tekstu wielkimi literami"
simple_title:         "Zapisywanie tekstu wielkimi literami"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszym wpisie na blogu porozmawiamy o jednej z podstawowych operacji w języku programowania Javascript - zamianie małych liter na duże. Dlaczego warto poznać ten prosty trik? Cóż, jest to bardzo przydatna umiejętność w wielu aplikacjach internetowych, jak również w analizie i manipulacji tekstem. Czytaj dalej, aby dowiedzieć się, jak to zrobić.

## Jak to zrobić

```Javascript
var example = "witaj świecie";
console.log(example.toUpperCase());
```

Kiedy uruchomisz ten kod, powinieneś zobaczyć na konsoli tekst "WITAJ ŚWIECIE" - zamienione litery na duże. Zauważ, że użyliśmy tutaj metody `toUpperCase()`, która jest wbudowaną funkcją w języku Javascript. Możesz ją wykorzystać do zmiany literek na duże w dowolnym ciągu znaków. Przetestuj tę metodę na różnych przykładach, a także spróbuj zamienić duże litery na małe za pomocą `toLowerCase()`.

Podczas zamiany liter na duże, warto również wiedzieć, że polskie znaki diakrytyczne również zostaną zmienione, nie tylko litery. Na przykład, litera `ś` zostanie zamieniona na `Ś`, a nie na `S`.

## Deep Dive

Ta prosta operacja może wydawać się trywialna, ale jest kilka ciekawych rzeczy, które warto o niej wiedzieć. Po pierwsze, warto zauważyć, że przypadki literek są bardzo ważne w języku Javascript. Oznacza to, że `Hello` jest zupełnie innym ciągiem znaków niż `hello`. Dzięki temu, przy wykonywaniu różnych operacji (takich jak porównywanie), jesteśmy w stanie rozróżnić między różnymi przypadkami słów.

Inną przydatną funkcją związaną z zamianą liter jest metoda `charAt()`, która pozwala nam uzyskać dostęp do konkretnego znaku w ciągu. Na przykład, jeśli chcemy uzyskać dostęp do pierwszej litery w wyrazie, musimy użyć `example.charAt(0)`, ponieważ liczymy znaki od zera.

## Zobacz również

- [Metoda toUpperCase() w języku Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Metoda toLowerCase() w języku Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Metoda charAt() w języku Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)