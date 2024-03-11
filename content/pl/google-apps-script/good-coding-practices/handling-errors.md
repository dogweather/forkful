---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:03.249739-07:00
description: "Obs\u0142uga b\u0142\u0119d\xF3w w Google Apps Script dotyczy przewidywania,\
  \ przechwytywania i reagowania na wyj\u0105tki lub b\u0142\u0119dy, kt\xF3re wyst\u0119\
  puj\u0105 podczas wykonywania skryptu.\u2026"
lastmod: '2024-03-11T00:14:08.082222-06:00'
model: gpt-4-0125-preview
summary: "Obs\u0142uga b\u0142\u0119d\xF3w w Google Apps Script dotyczy przewidywania,\
  \ przechwytywania i reagowania na wyj\u0105tki lub b\u0142\u0119dy, kt\xF3re wyst\u0119\
  puj\u0105 podczas wykonywania skryptu.\u2026"
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obsługa błędów w Google Apps Script dotyczy przewidywania, przechwytywania i reagowania na wyjątki lub błędy, które występują podczas wykonywania skryptu. Programiści implementują ją, aby zabezpieczyć skrypty przed nieoczekiwanymi awariami, zapewniając płynniejsze, przyjazne dla użytkownika aplikacje, które mogą elegancko zarządzać błędami lub rejestrować je bez nagłych awarii.

## Jak to zrobić:

Google Apps Script, bazując na JavaScript, pozwala nam używać tradycyjnego wyrażenia `try-catch` do obsługi błędów, wraz z `finally`, jeśli wymagane jest sprzątanie bez względu na sukces lub błąd.

```javascript
function myFunction() {
  try {
    // Kod, który może wygenerować błąd
    var sheet = SpreadsheetApp.getActiveSheet();
    var data = sheet.getRange("A1").getValue();
    if (data === "") {
      throw new Error("Komórka A1 jest pusta.");
    }
    Logger.log(data);
  } catch (e) {
    // Kod obsługi błędu
    Logger.log("Błąd: " + e.message);
  } finally {
    // Kod sprzątający, wykonany niezależnie od wystąpienia błędu
    Logger.log("Funkcja zakończona.");
  }
}
```

Przykładowe wyjście bez błędu:
```
[Wartość komórki]
Funkcja zakończona.
```

Przykładowe wyjście z błędem (zakładając, że A1 jest puste):
```
Błąd: Komórka A1 jest pusta.
Funkcja zakończona.
```

Google Apps Script wspiera również generowanie niestandardowych błędów za pomocą obiektu `Error` i przechwytywanie konkretnych typów błędów, jeśli jest to potrzebne. Jednak brak zaawansowanej kategoryzacji błędów czyni niezbędnym poleganie na komunikatach o błędach dla szczegółowości.

## Pogłębiona analiza

Historycznie, obsługa błędów w językach skryptowych, takich jak JavaScript (i przez rozszerzenie, Google Apps Script), była mniej zaawansowana niż w niektórych językach kompilowanych, które oferują funkcje takie jak szczegółowe hierarchie wyjątków i kompleksowe narzędzia debugowania. Model Google Apps Script jest stosunkowo prosty, wykorzystując paradigmat JavaScriptu `try-catch-finally`. Ta prostota wpisuje się w projekt języka mającego na celu szybki rozwój i wdrażanie aplikacji o małej do średniej skali w ekosystemie Google’a, ale czasami może ograniczać programistów mających do czynienia ze skomplikowanymi scenariuszami błędów.

W bardziej skomplikowanych aplikacjach programiści często uzupełniają natywną obsługę błędów Google Apps Script o niestandardowe mechanizmy logowania i raportowania błędów. Może to obejmować zapisywanie błędów do arkusza Google do celów audytu lub korzystanie z usług zewnętrznych rejestrujących błędy poprzez usługi URL Fetch w Google Apps Script, aby wysyłać szczegóły błędu poza środowisko skryptu.

Chociaż Google Apps Script może pozostawać w tyle za językami takimi jak Java czy C# pod względem wbudowanej złożoności i możliwości obsługi błędów, jego integracja z usługami Google oraz prostota podejścia `try-catch-finally` czynią go potężnym narzędziem dla programistów do szybkiego automatyzowania zadań i tworzenia integracji w ekosystemie Google. Programiści z innych środowisk mogą stwierdzić, że wyzwanie nie polega na opanowaniu skomplikowanych wzorców obsługi błędów, ale na kreatywnym wykorzystaniu dostępnego potencjału, aby zapewnić, że ich skrypty są solidne i przyjazne dla użytkownika.
