---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:45.722762-07:00
description: "Jak to zrobi\u0107: W Google Apps Script, kt\xF3ry oparty jest na JavaScripcie,\
  \ masz kilka podej\u015B\u0107 do parsowania daty ze \u0142a\u0144cucha znak\xF3\
  w. Poni\u017Cej przedstawiam\u2026"
lastmod: '2024-03-13T22:44:34.913685-06:00'
model: gpt-4-0125-preview
summary: "W Google Apps Script, kt\xF3ry oparty jest na JavaScripcie, masz kilka podej\u015B\
  \u0107 do parsowania daty ze \u0142a\u0144cucha znak\xF3w."
title: "Analiza sk\u0142adniowa daty z ci\u0105gu znak\xF3w"
weight: 30
---

## Jak to zrobić:
W Google Apps Script, który oparty jest na JavaScripcie, masz kilka podejść do parsowania daty ze łańcucha znaków. Poniżej przedstawiam przykłady użycia zarówno natywnych metod JavaScript, jak i narzędzi Google Apps Script.

**Używając konstruktora `new Date()`:**

Najprostszym sposobem na parsowanie łańcucha znaków na datę w Google Apps Script jest użycie konstruktora obiektu `Date`. Wymaga to jednak, aby łańcuch daty był w formacie rozpoznawanym przez metodę Date.parse() (np. RRRR-MM-DD).

```javascript
const dateString = '2023-04-01';
const dateObject = new Date(dateString);
Logger.log(dateObject); // Rejestruje Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)
```

**Używając `Utilities.parseDate()`:**

Aby uzyskać większą elastyczność, szczególnie w przypadku niestandardowych formatów dat, Google Apps Script oferuje `Utilities.parseDate()`. Metoda ta umożliwia określenie formatu daty, strefy czasowej i ustawień regionalnych.

```javascript
const dateString = '01-04-2023'; // DD-MM-RRRR
const format = 'dd-MM-yyyy';
const timezone = Session.getScriptTimeZone();
const dateObject = Utilities.parseDate(dateString, timezone, format);
Logger.log(dateObject); // Rejestruje Sat Apr 01 2023 00:00:00 GMT+0000 (UTC) w zależności od strefy czasowej skryptu
```

Uwaga: Podczas gdy `Utilities.parseDate()` oferuje większą kontrolę, jego zachowanie może różnić się w zależności od strefy czasowej skryptu, dlatego ważne jest, aby wyraźnie określić strefę czasową, jeśli twoja aplikacja obsługuje daty w wielu regionach.

## Dogłębna analiza
Parsowanie dat w językach programowania historycznie wiązało się z wyzwaniami, głównie z powodu różnorodności formatów dat i złożoności stref czasowych. Podejście Google Apps Script, oparte głównie na JavaScripcie, ma na celu uproszczenie tego, oferując zarówno prosty obiekt `Date`, jak i bardziej wszechstronną funkcję `Utilities.parseDate()`. Jednak każda metoda ma swoje ograniczenia; na przykład poleganie na konstruktorze `Date` ze łańcuchami znaków prowadzi do nieścisłości w różnych środowiskach ze względu na różne interpretacje formatów dat. Z drugiej strony, `Utilities.parseDate()` wymaga dokładniejszego zrozumienia formatu, strefy czasowej i ustawień regionalnych, co czyni go nieco bardziej skomplikowanym, ale bardziej niezawodnym dla konkretnych potrzeb.

Alternatywne biblioteki lub usługi, takie jak Moment.js (obecnie rekomendujący Luxon dla nowych projektów), oferują bogatsze funkcjonalności i lepsze obsługi stref czasowych, adresując wiele z tych wyzwań. Jednak w kontekście Google Apps Script, gdzie biblioteki zewnętrzne mają ograniczenia, zrozumienie i efektywne wykorzystanie wbudowanych metod staje się kluczowe. Programiści pochodzący z innych języków mogą uznać niuanse obsługi dat w Google Apps Script za wyjątkowo trudne, ale mogą osiągnąć solidne parsowanie dat dzięki głębokiemu zrozumieniu dostępnych narzędzi i starannemu rozważeniu globalnego charakteru swoich aplikacji.
