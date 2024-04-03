---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:50.434038-07:00
description: "Jak to zrobi\u0107: W Google Apps Script, kt\xF3re opiera si\u0119 na\
  \ nowoczesnym JavaScript, wyodr\u0119bnienie podci\u0105g\xF3w mo\u017Cna osi\u0105\
  gn\u0105\u0107 za pomoc\u0105 kilku metod, w tym\u2026"
lastmod: '2024-03-13T22:44:34.889766-06:00'
model: gpt-4-0125-preview
summary: "W Google Apps Script, kt\xF3re opiera si\u0119 na nowoczesnym JavaScript,\
  \ wyodr\u0119bnienie podci\u0105g\xF3w mo\u017Cna osi\u0105gn\u0105\u0107 za pomoc\u0105\
  \ kilku metod, w tym `substring()`, `substr()` i `slice()`."
title: "Wydobywanie podci\u0105g\xF3w"
weight: 6
---

## Jak to zrobić:
W Google Apps Script, które opiera się na nowoczesnym JavaScript, wyodrębnienie podciągów można osiągnąć za pomocą kilku metod, w tym `substring()`, `substr()` i `slice()`. Każda z nich ma swoje niuanse, ale wszystkie służą celowi wyciągania określonych znaków z ciągu.

```javascript
// Przykład użycia substring()
var str = "Hello, world!";
var result = str.substring(0, 5);
console.log(result); // Wyjście: Hello

// Przykład użycia substr()
var resultSubstr = str.substr(7, 5);
console.log(resultSubstr); // Wyjście: world

// Przykład użycia slice()
var resultSlice = str.slice(-6);
console.log(resultSlice); // Wyjście: world!
```

Każda metoda przyjmuje dwa argumenty: pozycję startową i, z wyjątkiem `slice()`, która może przyjmować indeksy ujemne, aby zacząć od końca, pozycję końcową lub liczbę znaków do wyodrębnienia. Warto zauważyć, że oryginalny ciąg pozostaje niezmieniony po tych operacjach, ponieważ zwracają one nowe wartości ciągów.

## Szczegółowa analiza
Historycznie metody JavaScript do wyodrębniania podciągów były źródłem zamieszania ze względu na ich podobne nazwy i funkcjonalności. Jednakże, w Google Apps Script i nowoczesnym JavaScript, `substring()` i `slice()` są najczęściej używane, z `substr()` uważanym za przestarzały. Jest to ważne do zauważenia dla osób piszących kod z myślą o przyszłości.

Główna różnica między `substring()` a `slice()` polega na tym, jak obsługują one indeksy ujemne; `substring()` traktuje indeksy ujemne jako 0, podczas gdy `slice()` może przyjmować indeks ujemny, aby rozpocząć ekstrakcję od końca ciągu. Dzięki temu `slice()` jest szczególnie przydatny w przypadkach, gdy dokładna długość ciągu może nie być znana lub gdy potrzebny jest fragment z końca.

Decydując, którą metodę użyć do wyodrębniania podciągów, wybór często sprowadza się do konkretnych wymagań operacji (np. czy korzystne jest obsługiwanie indeksów ujemnych) i osobistych lub zespołowych standardów kodowania. Chociaż nie ma uniwersalnej najlepszej praktyki, zrozumienie subtelnych różnic i implikacji wydajnościowych może pomóc w podjęciu świadomej decyzji.
