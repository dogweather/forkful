---
title:    "TypeScript: Szukanie i zamienianie tekstu"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego

Wiele razy, podczas pracy nad projektem, musimy zmienić tekst w różnych plikach. Wyszukiwanie i zamiana jest niezbędnym narzędziem w naszym zapasie do wykonywania tego zadania. W tym artykule dowiesz się, jak użyć programu TypeScript do wyszukiwania i wymiany tekstu w plikach.

## Jak to zrobić

Załóżmy, że mamy plik tekstowy zawierający wiele wystąpień słowa "JavaScript", a my chcemy to zmienić na "TypeScript". Aby to zrobić, musimy użyć funkcji `replace()` wraz z wyrażeniami regularnymi. Przykładowy kod wygląda tak:

```TypeScript
const wyrazenie = /JavaScript/g  
const nowyTekst = "TypeScript"  
const result = staryTekst.replace(wyrazenie, nowyTekst)
```

W powyższym kodzie, używamy wyrażenia regularnego `/JavaScript/g`, które oznacza, że wszystkie wystąpienia słowa "JavaScript" zostaną zastąpione przez "TypeScript". Następnie przypisujemy nowy tekst do zmiennej `nowyTekst` i używamy funkcji `replace()` na naszym starym tekście. Ostatecznie, zmienna `result` będzie zawierać zmieniony tekst.

## Deep Dive

W przypadku bardziej złożonych zadań wyszukiwania i wymiany tekstu, możemy również wykorzystać wyrażenia regularne do określenia wzorców, które chcemy znaleźć i zastąpić. Na przykład, jeśli chcemy zmienić daty z formatu "DD/MM/RRRR" na "RRRR/MM/DD", możemy użyć wyrażenia regularnego `/(\d{2})\/(\d{2})\/(\d{4})/g`, które oznacza, że szukamy dwóch cyfr, ukośnika, dwóch cyfr, ukośnika i czterech cyfr. Następnie używamy funkcji `replace()` przy użyciu grup z wyrażenia regularnego, aby odpowiednio zmienić daty w tekście. 

## Zobacz również

1. [Dokumentacja TypeScript na temat używania wyrażeń regularnych](https://www.typescriptlang.org/docs/handbook/regexp.html)
2. [Inne sposoby na wyszukiwanie i wymianę tekstu w TypeScript](https://www.tutorialspoint.com/typescript/typescript_strings.htm#replacing-text)
3. [Wzorce wyrażeń regularnych dla zaawansowanych użytkowników](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions/Advanced_Syntax)