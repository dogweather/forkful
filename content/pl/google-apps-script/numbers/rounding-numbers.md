---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:10.134017-07:00
description: "Zaokr\u0105glanie liczb, podstawowe poj\u0119cie w programowaniu komputerowym,\
  \ polega na dopasowaniu liczby do najbli\u017Cszej liczby ca\u0142kowitej lub do\
  \ okre\u015Blonej\u2026"
lastmod: '2024-03-13T22:44:34.896907-06:00'
model: gpt-4-0125-preview
summary: "Zaokr\u0105glanie liczb, podstawowe poj\u0119cie w programowaniu komputerowym,\
  \ polega na dopasowaniu liczby do najbli\u017Cszej liczby ca\u0142kowitej lub do\
  \ okre\u015Blonej liczby miejsc dziesi\u0119tnych."
title: "Zaokr\u0105glanie liczb"
weight: 13
---

## Co i dlaczego?

Zaokrąglanie liczb, podstawowe pojęcie w programowaniu komputerowym, polega na dopasowaniu liczby do najbliższej liczby całkowitej lub do określonej liczby miejsc dziesiętnych. Programiści często wykonują zaokrąglanie, aby uprościć liczby dla czytelności człowieka lub spełnić konkretne potrzeby obliczeniowe, zapewniając precyzję i redukując obciążenie obliczeniowe.

## Jak:

Google Apps Script, będąc językiem opartym na JavaScript, oferuje standardowe metody zaokrąglania liczb. Oto przegląd trzech powszechnie używanych technik:

### Math.round()
Ta funkcja zaokrągla liczbę do najbliższej liczby całkowitej.

```javascript
var number = 2.56;
var roundedNumber = Math.round(number); 
Logger.log(roundedNumber); // Wyświetla: 3
```

### Math.ceil()
Zaokrągla liczbę w górę do najbliższej liczby całkowitej.

```javascript
var number = 2.56;
var roundedUp = Math.ceil(number); 
Logger.log(roundedUp); // Wyświetla: 3
```

### Math.floor()
Przeciwnie, zaokrągla liczbę w dół do najbliższej liczby całkowitej.

```javascript
var number = 2.56;
var roundedDown = Math.floor(number); 
Logger.log(roundedDown); // Wyświetla: 2
```

Dla określonej liczby miejsc dziesiętnych, można użyć `.toFixed()`, który tak naprawdę zwraca ciąg znaków, lub bardziej subtelne podejście do matematycznego zaokrąglania:

```javascript
var number = 2.56789;
var fixedNumber = number.toFixed(2); 
Logger.log(fixedNumber); // Wyświetla: "2.57" (jako ciąg znaków)

var preciseRound = Math.round(number * 100) / 100; 
Logger.log(preciseRound); // Wyświetla: 2.57
```

## Szczegółowa analiza

Zaokrąglanie liczb w Google Apps Script nie różni się wiele od tego, jak jest wykonywane w innych środowiskach JavaScript. Jednak zrozumienie różnic w metodach zaokrąglania i potencjalnych problemów z arytmetyką liczb zmiennoprzecinkowych jest kluczowe. Na przykład, ze względu na sposób, w jaki komputery reprezentują liczby zmiennoprzecinkowe, nie wszystkie ułamki dziesiętne mogą być reprezentowane z doskonałą dokładnością, co prowadzi do czasami nieoczekiwanych wyników zaokrąglenia.

Historycznie rzecz biorąc, JavaScript (a przez rozszerzenie Google Apps Script) radzi sobie z tym, stosując się do standardu IEEE 754, używanego przez wiele innych języków programowania do arytmetyki liczb zmiennoprzecinkowych. Ten standard określa, jak liczby są zaokrąglane, zapewniając spójność na różnych platformach i językach.

Chociaż bezpośrednie metody zaokrąglania w Google Apps Script są proste i często wystarczające, złożone aplikacje lub aplikacje wymagające wysokiej precyzji mogą skorzystać z bibliotek takich jak decimal.js czy big.js, które są zaprojektowane do obsługi arytmetyki o dowolnej precyzji. Mogą być one szczególnie przydatne przy pracy z obliczeniami finansowymi lub naukowymi, gdzie dokładność zaokrąglonych liczb ma kluczowe znaczenie.

Pamiętaj jednak, że korzystanie z zewnętrznych bibliotek w Google Apps Script wymaga ich wczytania przez edytor skryptów, co może wprowadzić zależności lub wpłynąć na wydajność Twojego skryptu w zależności od sposobu użycia. W wielu przypadkach wbudowane metody Math są całkowicie wystarczające, ale dla tych przypadków skrajnych, które wymagają precyzji do n-tego stopnia, spojrzenie poza standardową bibliotekę może być konieczne.
