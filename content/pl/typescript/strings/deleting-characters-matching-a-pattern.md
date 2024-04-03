---
date: 2024-01-20 17:43:26.048214-07:00
description: "Usuwanie znak\xF3w zgodnych z wzorcem to filtracja ci\u0105gu tekstowego\
  \ przez usuni\u0119cie okre\u015Blonych znak\xF3w. Programi\u015Bci robi\u0105 to,\
  \ aby oczy\u015Bci\u0107 dane wej\u015Bciowe,\u2026"
lastmod: '2024-03-13T22:44:35.122073-06:00'
model: gpt-4-1106-preview
summary: "Usuwanie znak\xF3w zgodnych z wzorcem to filtracja ci\u0105gu tekstowego\
  \ przez usuni\u0119cie okre\u015Blonych znak\xF3w."
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## How to: (Jak to zrobić?)
```TypeScript
function deleteMatchingCharacters(str: string, pattern: RegExp): string {
  return str.replace(pattern, '');
}

// Przykład użycia:
const originalString = 'Hello, World! 1234';
const cleanedString = deleteMatchingCharacters(originalString, /[0-9,!]/g);

console.log(cleanedString); // Wypisze: "Hello World"
```
Zauważ, że używamy `RegExp` (klasa wyrażeń regularnych w JavaScript) do definiowania wzorca znaków do usunięcia oraz globalnego flagi `g` do znalezienia wszystkich wystąpień. Metoda `replace` przeszukuje i zastępuje znalezione pasujące fragmenty.

## Deep Dive (Dogłębna analiza)
Usuwanie znaków pasujących do wzorca nie jest nowym pomysłem – wywodzi się z koncepcji wyrażeń regularnych, które narodziły się w teorii automatów i języków formalnych. Koncept ten znalazł swoje zastosowanie na początku lat 70. w edytorach tekstu i narzędziach Unix.

Dzisiaj alternatywą do `replace` z wyrażeniami regularnymi może być na przykład użycie metod `split` i `filter` na łańcuchach znaków. Jednakże, zazwyczaj jest to mniej wydajne.

Implementacja w TypeScript jest prosta, ale pamiętaj o bezpieczeństwie. Niebezpieczne wzorce mogą prowadzić do ataków, takich jak ReDoS (Regular Expression Denial of Service). Uważaj na specjalne znaki w wyrażeniach regularnych i zawsze testuj performance.

## See Also (Zobacz również)
- MDN Web Docs on Regular Expressions: [Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- RegExp Pattern Syntax: [RegExp Patterns](https://www.regular-expressions.info/javascript.html)
- TypeScript Handbook: [TypeScript Handbook](https://www.typescriptlang.org/docs/)
- Performance considerations: [ReDoS](https://owasp.org/www-community/attacks/Regular_expression_Denial_of_Service_-_ReDoS)
