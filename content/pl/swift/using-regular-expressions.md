---
title:    "Swift: Korzystanie z wyrażeń regularnych"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą i chcesz manipulować tekstem w swoim kodzie, ale nie chcesz pisać dużej ilości warunków i funkcji, regular expressions są idealnym rozwiązaniem. Pozwalają one na wyszukiwanie, porównywanie i modyfikowanie tekstu w łatwy i szybki sposób, co oszczędza czas i zapewnia większą precyzję w przetwarzaniu danych.

## Jak używać

Aby używać regular expressions w języku Swift, musisz zaimportować moduł Foundation i użyć konstruktora "try?". Następnie możesz użyć funkcji "range (of:)" do ustalenia, czy dany wyrażenie jest obecne w tekście. Poniższy przykład pokazuje, jak sprawdzić, czy ciąg "Swift" znajduje się w zmiennej "text".

```Swift 
import Foundation 

let text = "Uczę się programowania w Swift" 

if let range = try? NSRegularExpression(pattern: "Swift"). 
  range(of: text) { 
    print("Znaleziono wyrażenie") 
  } else { 
    print("Nie znaleziono wyrażenia") 
  } 
```

To tylko podstawowy przykład wykorzystania regular expressions w Swift, ale istnieje wiele innych funkcji, takich jak "replaceMatches (in:)" czy "numberOfMatches (in:)", które mogą być przydatne w różnych przypadkach.

## Głębszy zanurzenie

Regular expressions w Swift są oparte na standardowych wyrażeniach regularnych, więc warto zapoznać się z ich składnią, aby lepiej zrozumieć sposób działania narzędzia. Możesz także użyć dodatkowych atrybutów i flag, takich jak "caseInsensitive" czy "ignoreMetacharacters", aby dostosować wyszukiwanie do swoich potrzeb.

## Zobacz także

- [Dokumentacja Apple na temat regular expressions w Swift](https://developer.apple.com/documentation/foundation/nsregularexpression?language=objc)
- [Poradnik na temat wyrażeń regularnych w Swift](https://www.swiftbysundell.com/articles/regular-expressions-in-swift/)
- [Przykłady użycia regular expressions w codziennych problemach programistycznych](https://regex101.com/)