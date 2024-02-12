---
title:                "Pobieranie strony internetowej"
aliases:
- /pl/google-apps-script/downloading-a-web-page/
date:                  2024-02-01T21:52:44.835765-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pobieranie strony internetowej"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/google-apps-script/downloading-a-web-page.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobieranie strony internetowej w Google Apps Script polega na pobieraniu treści strony internetowej za pomocą HTML do różnych celów, takich jak web scraping, ekstrakcja danych lub monitorowanie zmian. Programiści optują za tą operację, aby automatyzować zadania związane z gromadzeniem danych lub integracją, minimalizując wysiłek manualny i zapewniając przetwarzanie danych w czasie rzeczywistym.

## Jak to zrobić:

W Google Apps Script, usługa `UrlFetchApp` jest kluczowa do pobierania treści internetowych. Poniżej znajduje się przewodnik krok po kroku oraz prosty przykład demonstrujący, jak pobrać i zalogować treść HTML strony internetowej:

1. **Podstawowa operacja pobierania:**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- Ten kod pobiera treść HTML example.com i rejestruje ją. Jest to prosta demonstracja uzyskania źródła strony internetowej bez żadnych dodatkowych parametrów.

2. **Obsługa przekierowań i HTTPS:**

Dla HTTPS lub obsługi przekierowań, kod pozostaje w dużej mierze taki sam, ale rozważ implementację obsługi błędów lub konkretnych opcji dla przekierowań:

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // Automatyczne śledzenie przekierowań
    'muteHttpExceptions': true // Wyciszenie możliwych wyjątków dla ich łagodnego obsługiwania
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **Limity użycia i kwoty:**

Miej na uwadze limity Google Apps Script; intensywne użycie może wymagać obsługi błędów dla limitów szybkości.

## Wgłębienie się

Historycznie, pobieranie i manipulacja treścią internetową zaczęły się od prostych żądań HTTP, ewoluując znacząco z nadejściem języków skryptowych. Google Apps Script umożliwia proste wykonanie takich zadań w ekosystemie G Suite, wykorzystując solidną infrastrukturę Google. Usługa `UrlFetchApp` jest kluczowym elementem tej funkcjonalności, zakapsułowującym złożone żądania HTTP/S w prostszy interfejs na poziomie aplikacji.

Pomimo swojej wygody, Google Apps Script może nie zawsze być najlepszym narzędziem do intensywnego web scrapingu lub gdy wymagane jest złożone post-processing pobranych danych ze względu na limity czasu wykonania i kwoty nałożone przez Google. W takich przypadkach, dedykowane ramy do web scrapingu lub języki zaprojektowane do operacji I/O asynchronicznych, takie jak Node.js z bibliotekami typu Puppeteer czy Cheerio, mogą oferować większą elastyczność i moc.

Ponadto, chociaż Google Apps Script jest doskonałym narzędziem do integracji z Usługami Google (takimi jak Arkusze, Dokumenty i Dysk) i wykonywania lekkich operacji pobierania danych, istotne jest pamiętanie o ograniczeniach środowiska wykonawczego. Dla zadań intensywnych, rozważ użycie Google Cloud Functions lub zaawansowanych usług Apps Script z zewnętrznymi zasobami obliczeniowymi do przetwarzania.
