---
title:                "Zamiana liter w ciągu na wielkie"
aliases:
- /pl/google-apps-script/capitalizing-a-string.md
date:                  2024-02-01T21:49:03.289636-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zamiana liter w ciągu na wielkie"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/google-apps-script/capitalizing-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Capitalizacja ciągu polega na modyfikacji wejścia tak, aby pierwsza litera była wielką literą, a reszta małymi literami, co jest powszechnie stosowane do formatowania nazw lub tytułów. Programiści robią to, aby zapewnić spójność danych i poprawić czytelność w interfejsach użytkownika lub dokumentach.

## Jak to zrobić:

Google Apps Script, oparty na JavaScript, umożliwia kilka metod na kapitalizację ciągu, chociaż nie posiada wbudowanej funkcji. Oto kilka zwięzłych przykładów:

**Metoda 1: Użycie charAt() i slice()**

```javascript
function capitalizeString(inputString) {
  if (!inputString) return '';
  return inputString.charAt(0).toUpperCase() + inputString.slice(1).toLowerCase();
}

// Przykładowe użycie
let result = capitalizeString('witaj, świecie');
console.log(result);  // Wynik: Witaj, świecie
```

**Metoda 2: Użycie wyrażenia regularnego**

Dla tych, którzy preferują rozwiązanie oparte na wyrażeniach regularnych, aby elegancko radzić sobie z przypadkami brzegowymi:

```javascript
function capitalizeStringRegex(inputString) {
  return inputString.toLowerCase().replace(/^\w/, c => c.toUpperCase());
}

// Przykładowe użycie
let result = capitalizeStringRegex('witaj, świecie');
console.log(result);  // Wynik: Witaj, świecie
```

Obie metody zapewniają, że pierwsza litera ciągu jest wielka, a reszta małe, co jest odpowiednie dla różnych zastosowań, w tym między innymi manipulacji w Google Sheets lub edycji dokumentów za pomocą Apps Script.

## Głębsze zagłębienie

Kapitalizacja ciągów w Google Apps Script jest prosta dzięki potężnym możliwościom manipulacji ciągami w JavaScript. Historycznie, języki takie jak Python oferują wbudowane metody takie jak `.capitalize()`, aby to osiągnąć, co stanowi niewielki dodatkowy krok dla programistów JavaScript i Apps Script. Jednak brak wbudowanej funkcji w JavaScript/Google Apps Script zachęca do elastyczności i głębszego zrozumienia technik manipulacji ciągami.

W skomplikowanych scenariuszach, takich jak kapitalizacja każdego słowa w ciągu (Title Case), programiści mogą łączyć metody regex z funkcjami `split()` i `map()`, aby przetworzyć każde słowo indywidualnie. Choć Google Apps Script nie oferuje bezpośredniej metody na kapitalizację ciągów, użycie istniejących metod manipulacji ciągami w JavaScript zapewnia dużą elastyczność, umożliwiając programistom skuteczne obsługiwanie ciągów zgodnie z ich konkretnymi potrzebami.

W przypadkach, gdy wydajność i efektywność są najważniejsze, warto zauważyć, że bezpośrednia manipulacja ciągami może być bardziej wydajna niż regex, szczególnie dla dłuższych ciągów lub operacji w dużych pętlach. Jednak dla większości praktycznych zastosowań w Google Apps Script, oba podejścia zapewniają niezawodne rozwiązania.
