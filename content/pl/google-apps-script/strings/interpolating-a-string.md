---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:48.843186-07:00
description: "Jak to zrobi\u0107: W Google Apps Script interpolacja ci\u0105g\xF3\
  w jest realizowana za pomoc\u0105 litera\u0142\xF3w szablonowych. S\u0105 to litera\u0142\
  y ci\u0105g\xF3w umo\u017Cliwiaj\u0105ce osadzanie\u2026"
lastmod: '2024-03-13T22:44:34.886130-06:00'
model: gpt-4-0125-preview
summary: "W Google Apps Script interpolacja ci\u0105g\xF3w jest realizowana za pomoc\u0105\
  \ litera\u0142\xF3w szablonowych."
title: "Interpolacja ci\u0105gu znak\xF3w"
weight: 8
---

## Jak to zrobić:
W Google Apps Script interpolacja ciągów jest realizowana za pomocą literałów szablonowych. Są to literały ciągów umożliwiające osadzanie wyrażeń, oznaczone za pomocą cudzysłowów zwróconych (\`) zamiast zwykłych cudzysłowów. Oto jak możesz ich używać:

```javascript
// Podstawowy przykład
function basicInterpolationExample() {
  const user = 'Alice';
  console.log(`Cześć, ${user}!`); // Wynik: Cześć, Alice!
}

// Używanie wyrażeń
function expressionInterpolationExample() {
  const a = 5;
  const b = 10;
  console.log(`Pięć plus dziesięć to ${a + b}.`); // Wynik: Pięć plus dziesięć to 15.
}

// Wieloliniowe ciągi
function multiLineStringExample() {
  const item = 'Google Apps Script';
  console.log(`To jest wieloliniowy ciąg:
Cześć wszystkim,
Dziś dyskutujemy o ${item}.`);
  // Wynik:
  // To jest wieloliniowy ciąg:
  // Cześć wszystkim,
  // Dziś dyskutujemy o Google Apps Script.
}

basicInterpolationExample();
expressionInterpolationExample();
multiLineStringExample();
```

Te przykłady ilustrują podstawowe użycie, osadzanie wyrażeń i tworzenie wieloliniowych ciągów z interpolowanymi wartościami.

## Pogłębiona analiza
Literały szablonowe, w tym interpolacja ciągów, zostały wprowadzone w ECMAScript 2015 (ES6) i następnie przyjęte w Google Apps Script. Przed tym programiści musieli polegać wyłącznie na konkatenacji ciągów, co mogło być uciążliwe dla skomplikowanych ciągów lub przy integrowaniu wielu wartości zmiennych.

```javascript
// Stary sposób (przed ES6)
var user = 'Bob';
console.log('Cześć, ' + user + '!');
```

Chociaż interpolacja ciągów jest potężną funkcją, ważne jest, aby być świadomym kontekstów, w których jest używana. Na przykład, bezpośrednie osadzenie danych wejściowych użytkownika bez odpowiedniej sanacji może prowadzić do problemów z bezpieczeństwem, takich jak ataki iniekcji. Deweloperzy Google Apps Script powinni upewnić się, że wszelkie dynamiczne treści interpolowane do ciągów są odpowiednio sprawdzane lub oczyszczone.

W porównaniu do innych języków programowania, koncepcja interpolacji ciągów istnieje powszechnie, z różną składnią. Python używa f-stringów lub metody `format`, Ruby używa `#{}` w podwójnie cytowanych ciągach, a wiele nowoczesnych języków przyjęło podobne funkcje ze względu na czytelność i wygodę, które oferują.

Chociaż Google Apps Script nie oferuje dodatkowych funkcji interpolacji poza tymi, które są dostarczane przez standardy ECMAScript, obecna funkcjonalność jest potężna i wystarczająca dla większości przypadków użycia. Deweloperzy pochodzący z języków z bardziej rozbudowanymi mechanizmami interpolacji mogą potrzebować dostosować swoje oczekiwania, ale prawdopodobnie docenią prostotę i efektywność literałów szablonowych w Google Apps Script.
