---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:52.623620-07:00
description: "Jak to zrobi\u0107: W Google Apps Script, aby wys\u0142a\u0107 \u017C\
  \u0105danie HTTP z uwierzytelnieniem podstawowym, wykorzystuje si\u0119 us\u0142\
  ug\u0119 `UrlFetchApp` w po\u0142\u0105czeniu z\u2026"
lastmod: '2024-03-13T22:44:34.902499-06:00'
model: gpt-4-0125-preview
summary: "W Google Apps Script, aby wys\u0142a\u0107 \u017C\u0105danie HTTP z uwierzytelnieniem\
  \ podstawowym, wykorzystuje si\u0119 us\u0142ug\u0119 `UrlFetchApp` w po\u0142\u0105\
  czeniu z nag\u0142\xF3wkiem autoryzacji zakodowanym w base64."
title: "Wysy\u0142anie \u017C\u0105dania HTTP z podstawowym uwierzytelnianiem"
weight: 45
---

## Jak to zrobić:
W Google Apps Script, aby wysłać żądanie HTTP z uwierzytelnieniem podstawowym, wykorzystuje się usługę `UrlFetchApp` w połączeniu z nagłówkiem autoryzacji zakodowanym w base64. Oto krok po kroku:

1. **Kodowanie poświadczeń**: Najpierw zakoduj swoją nazwę użytkownika i hasło w base64. Google Apps Script nie posiada natywnej funkcji kodowania base64 dla ciągów, dlatego użyjesz Utilities.base64Encode do tego celu.

```javascript
var username = 'TwojaNazwaUzytkownika';
var password = 'TwojeHaslo';
var encodedCredentials = Utilities.base64Encode(username + ':' + password);
```

2. **Ustawienie opcji żądania**: Mając gotowe zakodowane poświadczenia, przygotuj obiekt opcji dla żądania HTTP, w tym metodę i nagłówki.

```javascript
var options = {
  method: 'get', // lub 'post', 'put', w zależności od potrzeb
  headers: {
    'Authorization': 'Basic ' + encodedCredentials
  }
  // dodatkowe opcje, takie jak 'muteHttpExceptions' dla obsługi błędów, mogą być dodane tutaj
};
```

3. **Wykonanie żądania**: Użyj metody `UrlFetchApp.fetch` z adresem URL docelowym i obiektem opcji.

```javascript
var url = 'https://example.com/api/resource';
var response = UrlFetchApp.fetch(url, options);
Logger.log(response.getContentText());
```

Przykładowe wyjście po udanym żądaniu będzie się różnić w zależności od odpowiedzi API. Dla API opartego na JSON, możesz zobaczyć coś takiego:

```
{"status":"Success","data":"Dane zasobu tutaj..."}
```

Upewnij się, że obsłużysz możliwe błędy HTTP, sprawdzając kod odpowiedzi lub używając opcji `muteHttpExceptions` dla bardziej kontrolowanego zarządzania błędami.

## Pogłębiona analiza
Wysyłanie żądania HTTP z uwierzytelnieniem podstawowym jest standardową metodą w wielu językach programowania do uzyskiwania dostępu do zasobów sieciowych wymagających uwierzytelnienia. W kontekście Google Apps Script, `UrlFetchApp` oferuje prosty sposób na wykonywanie tych żądań HTTP, w tym tych wymagających uwierzytelnienia. Dołączenie podstawowych poświadczeń w nagłówkach żądania jest prostym, ale skutecznym sposobem, jednakże wiąże się z zastrzeżeniami dotyczącymi bezpieczeństwa, głównie dlatego, że poświadczenia są wysyłane w postaci zwykłego tekstu, tylko zakodowane w base64, co można łatwo zdekodować, jeśli zostaną przechwycone.

Dla lepszego bezpieczeństwa, rekomendowane są alternatywy takie jak OAuth 2.0, zwłaszcza przy pracy z danymi wrażliwymi lub operacjach. Google Apps Script posiada wbudowane wsparcie dla OAuth 2.0 z biblioteką `OAuth2`, ułatwiając proces uwierzytelniania przeciwko usługom obsługującym ten protokół.

Pomimo jego ograniczeń bezpieczeństwa, uwierzytelnienie podstawowe pozostaje szeroko stosowane dla prostych lub wewnętrznych aplikacji nieeksponowanych na szerszy internet. Jest łatwe do implementacji, ponieważ wymaga tylko jednego żądania z odpowiednio ustawionymi nagłówkami, co czyni je atrakcyjną opcją dla szybkich integracji lub dla API, gdzie wyższe metody zabezpieczeń nie są dostępne. Jednak programiści są zachęcani do rozważenia implikacji bezpieczeństwa i poszukiwania bezpieczniejszych alternatyw, kiedy są dostępne.
