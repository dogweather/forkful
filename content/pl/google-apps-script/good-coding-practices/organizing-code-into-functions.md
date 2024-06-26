---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:40.408627-07:00
description: "Jak to zrobi\u0107: W Google Apps Script, kt\xF3ry opiera si\u0119 na\
  \ JavaScript, definiujesz funkcje, u\u017Cywaj\u0105c s\u0142owa kluczowego `function`,\
  \ po kt\xF3rym nast\u0119puje\u2026"
lastmod: '2024-03-13T22:44:34.909040-06:00'
model: gpt-4-0125-preview
summary: "W Google Apps Script, kt\xF3ry opiera si\u0119 na JavaScript, definiujesz\
  \ funkcje, u\u017Cywaj\u0105c s\u0142owa kluczowego `function`, po kt\xF3rym nast\u0119\
  puje unikalna nazwa funkcji, nawiasy `()` mog\u0105ce zawiera\u0107 parametry i\
  \ nawiasy klamrowe `{}`, kt\xF3re zawieraj\u0105 blok kodu funkcji."
title: Organizacja kodu w funkcje
weight: 18
---

## Jak to zrobić:
W Google Apps Script, który opiera się na JavaScript, definiujesz funkcje, używając słowa kluczowego `function`, po którym następuje unikalna nazwa funkcji, nawiasy `()` mogące zawierać parametry i nawiasy klamrowe `{}`, które zawierają blok kodu funkcji. Oto podstawowy przykład:

```javascript
function greetUser() {
  var user = Session.getActiveUser().getEmail();
  Logger.log('Cześć, ' + user + '!');
}

greetUser();
```

Przykładowy wynik:

```
Cześć, someone@example.com!
```

Teraz rozważmy bardziej praktyczny przykład związany z Google Sheets, gdzie funkcjonalność dzielimy na dwie funkcje: jedną do konfiguracji arkusza i drugą do wypełniania go danymi.

```javascript
function setupSheet() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var sheet = ss.getSheets()[0];
  sheet.setName('Dane Sprzedaży');
  sheet.appendRow(['Produkt', 'Ilość', 'Cena']);
}

function populateSheet(data) {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName('Dane Sprzedaży');
  data.forEach(function(row) {
    sheet.appendRow(row);
  });
}

// Inicjalizacja tablicy danych
var salesData = [
  ['Widgety', 15, 2.5],
  ['Gadżety', 8, 3.75]
];

// Uruchomienie funkcji
setupSheet();
populateSheet(salesData);
```

W tym przykładzie `setupSheet` przygotowuje arkusz, a `populateSheet` zapełnia arkusz tablicą danych sprzedaży. Oddzielenie tych kwestii sprawia, że kod jest czystszy i bardziej elastyczny pod kątem zmian.

## Głębsze zanurzenie
Koncept dzielenia kodu na funkcje nie jest nowy ani unikatowy dla Google Apps Script; jest to fundamentalna praktyka programistyczna, popierana w prawie wszystkich językach programowania. Historycznie, funkcje ewoluowały z matematycznego konceptu mapowania wejść na wyjścia, co stało się kamieniem węgielnym programowania strukturalnego. To podejście promuje modularność i ponowne wykorzystanie kodu, oferując jasne ścieżki do testowania poszczególnych części skryptu.

Google Apps Script, opierając się na JavaScript, znacząco korzysta z funkcji pierwszoklasowych JavaScriptu, pozwalając na przekazywanie funkcji jako argumentów, zwracanie ich z innych funkcji i przypisywanie do zmiennych. Ta funkcja otwiera zaawansowane wzorce takie jak wywołania zwrotne i programowanie funkcyjne, chociaż te wzorce mogą wprowadzać złożoność, która może być niepotrzebna dla prostych zadań automatyzacji w Google Apps Script.

Dla większych projektów lub bardziej skomplikowanych aplikacji, deweloperzy mogą badać nowsze funkcje JavaScript, takie jak funkcje strzałkowe, async/await dla operacji asynchronicznych, a nawet TypeScript dla statycznego typowania. TypeScript, w szczególności, może być kompilowany do działania jako Google Apps Script, dostarczając deweloperom drogę do poszukiwania bardziej rygorystycznych sprawdzeń typów i zaawansowanych funkcji zorientowanych obiektowo.

Jednakże, dla większości potrzeb skryptowych w suite Google Apps, trzymanie się prostych, dobrze zorganizowanych funkcji, jak pokazano, zapewnia solidne podstawy. To zawsze jest kwestia balansu między wykorzystywaniem zaawansowanych funkcji dla efektywności a utrzymaniem prostoty dla łatwości utrzymania i czytelności.
