---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:50:01.452770-07:00
description: "Jak to zrobi\u0107: W Google Apps Script daty por\xF3wnuje si\u0119\
  \ za pomoc\u0105 obiekt\xF3w Date JavaScript, co umo\u017Cliwia korzystanie z standardowych\
  \ metod do oceny, kt\xF3ra\u2026"
lastmod: '2024-03-13T22:44:34.916995-06:00'
model: gpt-4-0125-preview
summary: "W Google Apps Script daty por\xF3wnuje si\u0119 za pomoc\u0105 obiekt\xF3\
  w Date JavaScript, co umo\u017Cliwia korzystanie z standardowych metod do oceny,\
  \ kt\xF3ra z dw\xF3ch dat jest wcze\u015Bniejsza, p\xF3\u017Aniejsza lub czy s\u0105\
  \ takie same."
title: "Por\xF3wnywanie dw\xF3ch dat"
weight: 27
---

## Jak to zrobić:
W Google Apps Script daty porównuje się za pomocą obiektów Date JavaScript, co umożliwia korzystanie z standardowych metod do oceny, która z dwóch dat jest wcześniejsza, późniejsza lub czy są takie same. Oto podstawowe podejście:

```javascript
function compareDates() {
  var date1 = new Date('2023-04-01T00:00:00');
  var date2 = new Date('2023-04-15T00:00:00');

  // Porównanie dat
  if (date1 < date2) {
    Logger.log('Date1 jest przed Date2');
  } else if (date1 > date2) {
    Logger.log('Date1 jest po Date2');
  } else {
    Logger.log('Obie daty są takie same');
  }
}

// Przykładowy wynik:
// Date1 jest przed Date2
```

Dla bardziej szczegółowych porównań (np. liczby dni między dwiema datami) można odjąć jedną datę od drugiej, co zwraca różnicę w milisekundach:

```javascript
function daysBetweenDates() {
  var date1 = new Date('2023-04-01');
  var date2 = new Date('2023-04-15');
  
  var różnica = date2 - date1;
  
  var dni = różnica / (1000 * 60 * 60 * 24); // Konwersja milisekund na dni
  Logger.log(dni + ' dni między datami');
}

// Przykładowy wynik:
// 14 dni między datami
```

## Dogłębna analiza
Google Apps Script wykorzystuje podstawowe zasady obiektów Date JavaScript dla porównywania dat, co jest fundamentalnym aspektem języka od jego początku. Użycie milisekund jako wartości porównawczej od Epoki Unixowej (1 stycznia 1970 roku) zapewnia wysoki poziom precyzji w określaniu różnic lub podobieństw między datami.

Chociaż to podejście jest skuteczne dla większości przypadków użycia w zakresie Google Apps Script, warto zauważyć, że operacje na datach — takie jak korekty stref czasowych i obliczenia roku przestępnego — czasami mogą prowadzić do zamieszania. Programiści z innych środowisk programistycznych (takich jak Python, gdzie moduły `datetime` i `dateutil` zapewniają bardziej subtelne traktowanie dat) mogą uznać obiekt Date JavaScript za mający niedostatki.

Dla złożonego obsługiwania i manipulacji datami, wykraczających poza proste porównania, biblioteki takie jak `Moment.js` (które nadal można używać w Google Apps Script przez zewnętrzne API) oferują bogaty zestaw funkcjonalności, które adresują te braki. Jednak natywny obiekt Date JavaScript nadal służy jako niezawodne narzędzie do większości zadań porównywania dat, szczególnie w kontekście Google Apps Script i jego integracji z zestawem aplikacji Google'a.
