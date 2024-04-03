---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:43.201249-07:00
description: "Konwersja ci\u0105g\xF3w znak\xF3w na ma\u0142e litery w Google Apps\
  \ Script, chmurowym j\u0119zyku skryptowym do automatyzacji zada\u0144 w produktach\
  \ Google, jest podstawowym\u2026"
lastmod: '2024-03-13T22:44:34.887342-06:00'
model: gpt-4-0125-preview
summary: "Konwersja ci\u0105g\xF3w znak\xF3w na ma\u0142e litery w Google Apps Script,\
  \ chmurowym j\u0119zyku skryptowym do automatyzacji zada\u0144 w produktach Google,\
  \ jest podstawowym zadaniem maj\u0105cym na celu standaryzacj\u0119 danych tekstowych."
title: "Konwersja \u0142a\u0144cucha znak\xF3w na ma\u0142e litery"
weight: 4
---

## Jak to zrobić:
Konwersja ciągów znaków na małe litery w Google Apps Script jest prosta, dzięki wbudowanym metodom JavaScript dostępnym w środowisku skryptowym. Metoda `toLowerCase()` to coś, czego będziesz głównie używać. Oto jak możesz ją zaimplementować:

```javascript
function convertToLower() {
  var originalString = "Hello, WORLD!";
  var lowerCaseString = originalString.toLowerCase();
  
  Logger.log(lowerCaseString); // Outputs: hello, world!
}
```

Ta prosta funkcja pokazuje przyjęcie oryginalnego ciągu znaków, zastosowanie metody `toLowerCase()` i zalogowanie wyniku. Jest to szczególnie przydatne, gdy mamy do czynienia z danymi wejściowymi, które muszą być niewrażliwe na wielkość liter. Na przykład przy porównywaniu adresów e-mail, które użytkownicy mogą wprowadzać w różnych przypadkach.

Dodatkowo, w sytuacjach, gdy pracujesz z danymi tablicowymi, możesz przemapować każdy element, aby przekonwertować je na małe litery:

```javascript
function convertArrayItemsToLower() {
  var namesArray = ["Alice", "BOB", "Charlie"];
  var lowerCaseNamesArray = namesArray.map(function(name) {
    return name.toLowerCase();
  });
  
  Logger.log(lowerCaseNamesArray); // Outputs: [alice, bob, charlie]
}
```

Ten przykład podkreśla wszechstronność `toLowerCase()` podczas pracy z wieloma danymi tekstowymi, zapewniając jednolitość w całym zestawie danych.

## Szczegółowa analiza
Metoda `toLowerCase()`, odziedziczona z JavaScript i wykorzystywana w Google Apps Script, była integralną częścią manipulacji ciągami znaków od wczesnych wersji JavaScript. Jej głównym celem jest pomoc w niewrażliwym na wielkość liter obsługiwaniu danych tekstowych, potrzeba, która pojawiła się wraz z pojawieniem się dynamicznych, interaktywnych aplikacji internetowych. Pomimo swojej prostoty, mechanizm ten odgrywa kluczową rolę w walidacji danych, sortowaniu i algorytmach wyszukiwania, redukując złożoność wprowadzoną przez uwzględnienie wielkości liter.

Pod względem wydajności proces konwersji jest wysoko optymalizowany w nowoczesnych silnikach JavaScript; jednak jego zastosowanie powinno być nadal roztropne w dużych operacjach na danych, aby uniknąć niepotrzebnego narzutu przetwarzania.

Alternatywą do rozważenia, szczególnie przy pracy z złożonymi wzorcami lub potrzebie konwersji specyficznej dla lokalizacji, jest metoda `toLocaleLowerCase()`. Ta wariant uwzględnia lokalne reguły konwersji znaków na małe litery, co może być istotne dla aplikacji obsługujących wiele języków:

```javascript
var stringWithUmlaut = "MÄRZ";
var lowerCaseUmlaut = stringWithUmlaut.toLocaleLowerCase('de-DE');

Logger.log(lowerCaseUmlaut); // Outputs: märz
```

Pomimo dodatkowej złożoności, `toLocaleLowerCase()` jest potężnym narzędziem dla międzynarodowych aplikacji, zapewniając, że konwersja szanuje normy lingwistyczne lokalizacji użytkownika. Bez względu na to, którą metodę wybierzesz, konwertowanie ciągów znaków na małe litery pozostaje kluczową częścią przetwarzania tekstu w Google Apps Script, łącząc różnice między danymi wejściowymi użytkownika a standaryzowanym obsługiwaniem danych.
