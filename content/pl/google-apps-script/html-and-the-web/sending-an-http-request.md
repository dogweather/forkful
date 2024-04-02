---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:51.941165-07:00
description: "Wysy\u0142anie \u017C\u0105dania HTTP w Google Apps Script polega na\
  \ programowym wywo\u0142aniu zewn\u0119trznego serwera internetowego lub API. Programi\u015B\
  ci robi\u0105 to, aby pobiera\u0107\u2026"
lastmod: '2024-03-13T22:44:34.899127-06:00'
model: gpt-4-0125-preview
summary: "Wysy\u0142anie \u017C\u0105dania HTTP w Google Apps Script polega na programowym\
  \ wywo\u0142aniu zewn\u0119trznego serwera internetowego lub API. Programi\u015B\
  ci robi\u0105 to, aby pobiera\u0107\u2026"
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

## Co i dlaczego?

Wysyłanie żądania HTTP w Google Apps Script polega na programowym wywołaniu zewnętrznego serwera internetowego lub API. Programiści robią to, aby pobierać lub wysyłać dane do usług internetowych, integrując ogromną sferę zasobów i funkcjonalności sieciowych bezpośrednio w swoich projektach Google Apps Script.

## Jak to zrobić:

W Google Apps Script podstawowym sposobem na wysłanie żądania HTTP jest użycie usługi `UrlFetchApp`. Usługa ta dostarcza metody do wykonywania żądań HTTP GET i POST. Oto prosty przykład wykonania żądania GET w celu pobrania danych JSON:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

Do wysłania żądania POST, które jest powszechnie używane do wysyłania danych do serwera, musisz dodać więcej szczegółów w parametrze opcji:

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    key1: 'value1',
    key2: 'value2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // Konwersja obiektu JavaScript na łańcuch JSON
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

Te fragmenty kodu pokazują podstawową realizację żądań GET i POST. Wynik będzie zależał od odpowiedzi API i można go zobaczyć w rejestrze Logger Google Apps Script.

## Pogłębiona analiza

Usługa `UrlFetchApp` w Google Apps Script znacznie ewoluowała od momentu swojego powstania, oferując bardziej zniuansowaną kontrolę nad żądaniami HTTP z funkcjami takimi jak ustawianie nagłówków, ładunku oraz obsługa multipart/form-data dla przesyłania plików. Choć zapewnia prosty sposób na integrację z zewnętrznymi usługami sieciowymi, deweloperzy pochodzący z bardziej rozbudowanych języków backendowych mogą uznać jej funkcjonalność za nieco ograniczoną w porównaniu z bibliotekami takimi jak `requests` w Pythonie czy `fetch` API w JavaScript w Node.js.

Jednym z widocznych ograniczeń jest limit czasu wykonania dla Google Apps Script, który wpływa na żądania o długim czasie wykonywania. Dodatkowo, chociaż `UrlFetchApp` obejmuje szeroki zakres przypadków użycia, bardziej złożone scenariusze, wymagające uwierzytelnienia OAuth lub obsługi bardzo dużych ładunków, mogą wymagać kreatywnych rozwiązań lub wykorzystania dodatkowych zasobów Google Cloud.

Mimo to, dla większości integracji, z którymi stykają się deweloperzy Google Workspace - od automatyzacji pobierania danych po publikowanie aktualizacji na zewnętrznych usługach - `UrlFetchApp` dostarcza potężne, dostępne narzędzie. Jego integracja z Google Apps Script oznacza, że nie ma potrzeby korzystania z zewnętrznych bibliotek czy skomplikowanej konfiguracji, co sprawia, że żądania HTTP są stosunkowo proste do wykonania w ramach ograniczeń Google Apps Script. W miarę jak krajobraz internetowych API ciągle się rozszerza, `UrlFetchApp` pozostaje kluczowym mostem dla programów Google Apps Script do interakcji ze światem poza ekosystemem Google.
