---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:45.950309-07:00
description: "Jak to zrobi\u0107: W Google Apps Script mo\u017Cesz znale\u017A\u0107\
  \ d\u0142ugo\u015B\u0107 ci\u0105gu znak\xF3w, u\u017Cywaj\u0105c w\u0142a\u015B\
  ciwo\u015Bci `.length`, podobnie jak w JavaScript. Ta w\u0142a\u015Bciwo\u015B\u0107\
  \ zwraca\u2026"
lastmod: '2024-03-13T22:44:34.891991-06:00'
model: gpt-4-0125-preview
summary: "W Google Apps Script mo\u017Cesz znale\u017A\u0107 d\u0142ugo\u015B\u0107\
  \ ci\u0105gu znak\xF3w, u\u017Cywaj\u0105c w\u0142a\u015Bciwo\u015Bci `.length`,\
  \ podobnie jak w JavaScript."
title: "Znajdowanie d\u0142ugo\u015Bci \u0142a\u0144cucha znak\xF3w"
weight: 7
---

## Jak to zrobić:
W Google Apps Script możesz znaleźć długość ciągu znaków, używając właściwości `.length`, podobnie jak w JavaScript. Ta właściwość zwraca liczbę znaków w ciągu, łącznie z odstępami i znakami specjalnymi. Oto kilka przykładów:

```javascript
// Zdefiniuj ciąg znaków
var text = "Hello, World!";
// Znajdź długość ciągu znaków
var length = text.length;
// Zaloguj długość
Logger.log(length); // Wynik: 13
```

W scenariuszach, gdy pracujesz z danymi wejściowymi użytkownika z Google Forms lub Sheets, znalezienie długości ciągu znaków pomaga w walidacji danych:

```javascript
// Przykładowe dane wejściowe od użytkownika w Google Sheets
var userEntry = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet().getRange("A1").getValue();
// Oblicz i zaloguj długość danych wejściowych
Logger.log(userEntry.length); // Wynik zależy od zawartości komórki A1
```

Przyjrzyjmy się praktycznemu przykładowi obejmującemu warunek. Jeśli dane wejściowe przekroczą określoną długość, możesz chcieć zgłosić błąd lub ostrzeżenie:

```javascript
var comment = "To jest przykładowy komentarz, który jest zbyt długi dla naszej bazy danych.";
if(comment.length > 50) {
  Logger.log("Błąd: Twój komentarz nie powinien przekraczać 50 znaków.");
} else {
  Logger.log("Dziękujemy za Twoją wiadomość.");
}
// Wynik: Błąd: Twój komentarz nie powinien przekraczać 50 znaków.
```

## Wnikliwiej
W kontekście Google Apps Script, który opiera się na JavaScript, właściwość `.length` pochodzi ze standardu ECMAScript, który reguluje specyfikacje JavaScript. Właściwość `.length` jest częścią JavaScript od jego wczesnych etapów, zapewniając prosty sposób na ocenę rozmiaru ciągu znaków.

Jednym godnym uwagi szczegółem jest to, że Google Apps Script jest wykonywany na serwerach Google, a nie w przeglądarce. Oznacza to, że gdy masz do czynienia z ciągami znaków i ich długością, zwłaszcza w dużych zbiorach danych pobranych z Google Sheets lub Docs, czas wykonania może być wpływany przez opóźnienia sieciowe i ograniczenia czasu wykonania skryptów.

Chociaż `.length` to prosty i powszechnie używany sposób na znalezienie długości ciągu znaków, alternatywne strategie mogą obejmować regex lub iterowanie przez ciąg w celu zliczenia znaków, zwłaszcza gdy masz do czynienia z wielobajtowymi znakami lub gdy musisz odfiltrować pewne rodzaje znaków. Jednakże, dla większości praktycznych celów w Google Apps Script, `.length` zapewnia niezawodny i wydajny sposób na określenie długości ciągu znaków.

Zawsze pamiętaj, zwłaszcza w Google Apps Script, aby brać pod uwagę kontekst, w którym uruchamiasz swój kod. Wydajność i ograniczenia wykonania mogą kierować Cię do optymalizacji procedur obsługi ciągów znaków, w tym sposobu, w jaki określasz ich długość.
