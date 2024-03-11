---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:50.434038-07:00
description: "Wyodr\u0119bnianie podci\u0105g\xF3w polega na wzi\u0119ciu cz\u0119\
  \u015Bci ci\u0105gu - w zasadzie tworzeniu nowego ci\u0105gu z cz\u0119\u015Bci\
  \ istniej\u0105cego. Programi\u015Bci robi\u0105 to z wielu powod\xF3w, w\u2026"
lastmod: '2024-03-11T00:14:08.060554-06:00'
model: gpt-4-0125-preview
summary: "Wyodr\u0119bnianie podci\u0105g\xF3w polega na wzi\u0119ciu cz\u0119\u015B\
  ci ci\u0105gu - w zasadzie tworzeniu nowego ci\u0105gu z cz\u0119\u015Bci istniej\u0105\
  cego. Programi\u015Bci robi\u0105 to z wielu powod\xF3w, w\u2026"
title: "Wydobywanie podci\u0105g\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyodrębnianie podciągów polega na wzięciu części ciągu - w zasadzie tworzeniu nowego ciągu z części istniejącego. Programiści robią to z wielu powodów, w tym do analizowania danych, manipulacji tekstem dla interfejsów użytkownika czy przetwarzania danych wejściowych dla różnych aplikacji, co sprawia, że ekstrakcja podciągów jest uniwersalnym narzędziem w arsenale skryptowym.

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
