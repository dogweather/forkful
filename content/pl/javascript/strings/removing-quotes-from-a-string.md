---
date: 2024-01-26 03:40:32.873795-07:00
description: "Jak to zrobi\u0107: Wyobra\u017A sobie, \u017Ce masz ci\u0105g znak\xF3\
  w otoczony podw\xF3jnymi cudzys\u0142owami, takimi jak `\"\\\"Hello, World!\\\"\"\
  ` i chcesz uzyska\u0107 czysty tekst bez\u2026"
lastmod: '2024-03-13T22:44:35.784517-06:00'
model: gpt-4-0125-preview
summary: "Wyobra\u017A sobie, \u017Ce masz ci\u0105g znak\xF3w otoczony podw\xF3jnymi\
  \ cudzys\u0142owami, takimi jak `\"\\\"Hello, World!\\\"\"` i chcesz uzyska\u0107\
  \ czysty tekst bez cudzys\u0142ow\xF3w."
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
weight: 9
---

## Jak to zrobić:
Wyobraź sobie, że masz ciąg znaków otoczony podwójnymi cudzysłowami, takimi jak `"\"Hello, World!\""` i chcesz uzyskać czysty tekst bez cudzysłowów. Oto szybki fragment kodu JavaScript, który uwolni twój ciąg znaków z tych łańcuchów cudzysłowu:

```javascript
let quotedString = "\"Hello, World!\"";
let unquotedString = quotedString.replace(/^"|"$/g, '');
console.log(unquotedString); // Wynik: Hello, World!
```

A jeśli masz do czynienia z pojedynczymi cudzysłowami? Wystarczy nieco zmodyfikować wyrażenie regularne:

```javascript
let singleQuotedString = "'Hello, World!'";
let unquotedString = singleQuotedString.replace(/^'|'$/g, '');
console.log(unquotedString); // Wynik: Hello, World!
```

A co jeśli twój ciąg znaków to mieszanka obu? Nie ma problemu:

```javascript
let mixedQuotedString = "\"'Hello, World!'\"";
let unquotedString = mixedQuotedString.replace(/^["']|["']$/g, '');
console.log(unquotedString); // Wynik: 'Hello, World!'
```

## W głąb tematu
Zanim JSON przejął kontrolę, unikanie cudzysłowów było dzikim zachodem pełnym ukośników wstecznych i obejść. Wczesne języki programowania nie zawsze dobrze radziły sobie z cudzysłowami, co oznaczało wiele ręcznej manipulacji ciągami znaków. Teraz, przy ustandaryzowanych formatach danych, usuwanie cudzysłowów często dotyczy oczyszczania danych wejściowych przed ich przetworzeniem jako JSON lub zapisywaniem tekstu bez konfliktów formatowania.

Alternatywy dla `.replace()`? Jasne! Możesz podzielić i połączyć ciąg znaków w miejscach cudzysłowów, użyć metody `slice`, jeśli jesteś pewien pozycji swoich cudzysłowów, albo nawet dopasować wyrażenia regularne, aby wyodrębnić potrzebny tekst. Wszystko zależy od kontekstu.

Ale nie zapomnij o przypadkach brzegowych: cudzysłowy w cudzysłowach, cudzysłowy uciekające i znaki międzynarodowe. Pomyśl o swoim ciągu znaków jako o potencjalnym polu minowym wyjątków i postępuj ostrożnie. Nowoczesne silniki JavaScript są zoptymalizowane do efektywnego przetwarzania operacji z wyrażeniami regularnymi, więc to zazwyczaj metoda pierwszego wyboru, ale zawsze warto sprawdzić wydajność przy zadaniach z intensywnym przetwarzaniem danych.

## Zobacz również
Zagłęb się w manipulację ciągami znaków i wyrażeniami regularnymi:

- Sieć deweloperów Mozilli na temat String.replace(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 do testowania Twoich wzorców wyrażeń regularnych: https://regex101.com/
- JSON.org, aby zrozumieć, dlaczego w nowoczesnym rozwoju webowym mamy do czynienia z tak wieloma cudzysłowami: http://json.org/
