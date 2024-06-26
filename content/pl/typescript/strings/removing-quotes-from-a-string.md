---
date: 2024-01-26 03:43:15.306261-07:00
description: "Jak to zrobi\u0107: Oto Tw\xF3j bezpo\u015Bredni przewodnik, jak uwolni\u0107\
  \ Twoje stringi od tych irytuj\u0105cych znak\xF3w cudzys\u0142owu w TypeScript."
lastmod: '2024-03-13T22:44:35.126298-06:00'
model: gpt-4-0125-preview
summary: "Oto Tw\xF3j bezpo\u015Bredni przewodnik, jak uwolni\u0107 Twoje stringi\
  \ od tych irytuj\u0105cych znak\xF3w cudzys\u0142owu w TypeScript."
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
weight: 9
---

## Jak to zrobić:
Oto Twój bezpośredni przewodnik, jak uwolnić Twoje stringi od tych irytujących znaków cudzysłowu w TypeScript.

```typescript
// Opcja A: Zastępowanie pojedynczych lub podwójnych cudzysłowów za pomocą regex
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"Quoted string"`)); // Quoted string
console.log(removeQuotes(`'Another one'`)); // Another one

// Opcja B: Radzenie sobie ze stringami, które zaczynają się i kończą na różne cudzysłowy
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"Mismatched'`)); // "Mismatched'

// Opcja C: Usuwanie wielu rodzajów cudzysłowów
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'Mix'n'Match'"`)); // Mix'n'Match
```

## Głębsze spojrzenie
Dawno, dawno temu, zanim TypeScript stał się rzeczą, programiści JavaScript już mierzyli się z cudzysłowowymi sztuczkami, i historia jest mniej więcej taka sama w przypadku TypeScript. Wraz ze zmianą czasów zmienia się także sposób, w jaki dzielimy stringi. Obecnie, z siłą mięśni regex, odsuwamy na bok uciążliwe cięcie stringów lub inne żmudne metody.

Chociaż powyższe przykłady powinny pokryć większość Twoich potrzeb, pamiętaj, że cytowanie może być skomplikowane. Zagnieżdżone, niepasujące do siebie i uciekające cudzysłowy to psotniki, które czekają, by Cię potknąć. W tych przypadkach, możesz potrzebować bardziej wyrafinowanych wzorców lub nawet parserów, aby poradzić sobie z każdym krętym przypadkiem.

Alternatywy? Niektórzy ludzie wolą korzystać z bibliotek takich jak lodash, z metodami takimi jak `trim` i `trimStart` / `trimEnd`, które można dostosować do przycinania cudzysłowów, jeśli ustalisz znaki, które chcesz przyciąć.

A dla entuzjastów TypeScript, nie zapominajmy o typach. Chociaż tutaj zajmujemy się głównie stringami, kiedy pracujesz z danymi wejściowymi użytkownika lub parsowaniem, wprowadzenie pewnych strażników typu lub nawet ogólników może pomóc zapewnić, że Twoje kody są tak bezpieczne, jak Twoje przycięte cudzysłowy.

## Zobacz również
Sprawdź te wirtualne miejscówki po więcej informacji:

- MDN Web Docs o regex (https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Wyrażenia_regularne)
- Oficjalna dokumentacja TypeScript (https://www.typescriptlang.org/docs/)
- Nie potrzebujesz Lodash/Underscore – Pomocnicy String (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: Przemierzaj okopy, gdzie niezliczeni deweloperzy walczyli z katastrofami cudzysłowowymi (https://stackoverflow.com/)
