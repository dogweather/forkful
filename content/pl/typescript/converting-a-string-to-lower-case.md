---
title:                "TypeScript: Zamiana ciągu znaków na małe litery"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele sytuacji, kiedy potrzebujemy zamienić ciąg znaków na małe litery. Na przykład może to być potrzebne przy weryfikacji danych użytkownika, aby uniknąć błędów związanych z wielkością liter. Jest to również ważne w przypadku porównywania ciągów znaków, ponieważ znaki nie są zależne od wielkości. W tym blogu dowiesz się, jak prostym sposobem możesz przekształcić ciąg znaków na małe litery w języku TypeScript.

## Jak to zrobić

```TypeScript
let str: string = "PRZYKŁADOWY CIĄG ZNAKÓW"
console.log(str.toLowerCase());
```
**Wynik:**
```
przykładowy ciąg znaków
```
W powyższym przykładzie utworzyliśmy zmienną str zawierającą nasz przykładowy ciąg znaków. Następnie użyliśmy metody `toLowerCase()` aby przekształcić ciąg znaków na małe litery. Metoda ta zwraca nowy ciąg znaków, więc musimy go przypisać do zmiennej, jeśli chcemy go wykorzystać w dalszej części kodu.

## Deep Dive

Dokładniej rzecz biorąc, metoda `toLowerCase()` przekształca wszystkie wielkie litery w ciągu znaków na ich odpowiedniki małych liter. Jest to przydatne w przypadku języków, w których są różnice między małymi i dużymi literami, takich jak język polski. Metoda ta również pomija znaki specjalne oraz liczby, ponieważ nie mają one odpowiadających sobie małych i wielkich liter.

Ponadto, w języku TypeScript, istnieje również metoda `toUpperCase()` która wykonuje odwrotną operację - przekształca wszystkie litery w ciągu na duże litery.

## Zobacz również

- Dokumentacja Microsoft na temat konwersji znaków: https://docs.microsoft.com/pl-pl/dotnet/api/system.string.tolower
- Przykładowy kod z wykorzystaniem toLowerCase(): https://www.tutorialspoint.com/typescript/typescript_string_tolowercase.htm 
- Porównanie między metodami toLowerCase() i toUpperCase(): https://www.freecodecamp.org/news/how-to-convert-a-string-to-uppercase-or-lowercase-in-javascript-49930eea3b8c/