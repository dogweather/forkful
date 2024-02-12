---
title:                "Analiza Składniowa HTML"
aliases:
- /pl/google-apps-script/parsing-html/
date:                  2024-02-01T21:57:26.651028-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza Składniowa HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/google-apps-script/parsing-html.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsowanie HTML w Google Apps Script polega na ekstrakcji danych z treści HTML, co jest szczególnie przydatne podczas interakcji ze stronami internetowymi lub źródłami danych opartymi na sieci. Programiści robią to, aby automatyzować zbieranie danych, manipulować zawartością sieciową lub integrować funkcjonalności sieciowe z aplikacjami Google, takimi jak Arkusze i Dokumenty.

## Jak to zrobić:
Google Apps Script nie posiada wbudowanej metody do parsowania HTML. Można jednak wykorzystać usługę `UrlFetchApp` do pobierania zawartości HTML, a następnie użyć metod JavaScript lub regex (wyrażeń regularnych) do parsowania. Poniżej znajduje się podstawowy przykład, jak pobrać i zanalizować tag tytułu ze strony internetowej.

```javascript
function parseHTMLTitle(url) {
  // Pobierz zawartość HTML strony internetowej
  const response = UrlFetchApp.fetch(url);
  const htmlContent = response.getContentText();

  // Użyj prostego regex, aby znaleźć zawartość tagu <title>
  const titleRegex = /<title>(.*?)<\/title>/;
  const match = htmlContent.match(titleRegex);

  // Sprawdź, czy tytuł został znaleziony i zwróć go
  if (match && match.length > 1) {
    return match[1];
  }

  return 'Nie znaleziono tytułu';
}

// Przykładowe użycie
const url = 'http://example.com';
const pageTitle = parseHTMLTitle(url);
Logger.log(pageTitle); // Wyświetla tytuł strony internetowej
```

Dla bardziej zaawansowanego parsowania HTML, można użyć `XmlService` do analizy HTML jako XML. Należy jednak pamiętać, że wymaga to, aby HTML był poprawnym XML, co nie zawsze ma miejsce:

```javascript
function parseHTMLUsingXmlService(htmlContent) {
  try {
    const document = XmlService.parse(htmlContent);
    const rootElement = document.getRootElement();
    // Stąd nawiguj po drzewie XML za pomocą metod XmlService
    // Na przykład, aby znaleźć określony element lub atrybut
  } catch(e) {
    Logger.log('Błąd przetwarzania HTML: ' + e.toString());
  }
}
```

## Pogłębiona analiza:
Historycznie, parsowanie HTML w środowiskach takich jak Google Apps Script było wyzwaniem ze względu na brak modelu obiektowego dokumentu (DOM) lub dedykowanych bibliotek do parsowania, które są powszechne w innych kontekstach programistycznych. JavaScript w przeglądarce, na przykład, ma łatwo dostępny DOM, a środowiska Node.js mają dostęp do wielu pakietów NPM, takich jak `cheerio` czy `jsdom`, do parsowania HTML.

Podejście Google Apps Script opiera się głównie na użyciu `UrlFetchApp` do żądań internetowych, a następnie manipulowaniu otrzymanymi danymi za pomocą regex lub metod parsowania XML. Chociaż regex może być przydatny do prostych zadań parsowania, zazwyczaj nie jest zalecany do skomplikowanego HTML ze względu na ryzyko błędów i potencjalnie kruchej natury kodu. Parsowanie XML za pomocą `XmlService` oferuje bardziej zorganizowane podejście, ale wymaga poprawnego HTML/XML, co może być ograniczeniem przy pracy z dowolnymi stronami internetowymi.

Dla złożonych potrzeb parsowania lub przy pracy z źle sformatowanym HTML, jedną ze strategii alternatywnych może być użycie usługi internetowej zewnętrznej względem Google Apps Script. Ta usługa mogłaby przetwarzać zawartość HTML, być może używając bardziej zaawansowanej techniki lub biblioteki do parsowania, a następnie zwracać przetworzone dane w formie łatwej do wykorzystania przez Google Apps Script. Podejście to, jednak wprowadza opóźnienia sieciowe i złożoność zarządzania dodatkową usługą internetową.

Pomimo tych wyzwań, parsowanie HTML w ramach Google Apps Script pozostaje potężnym narzędziem, szczególnie w połączeniu z innymi usługami i API Google, zapewniając szereg możliwości automatyzacji, które mogą znacznie zwiększyć produktywność i zdolności przetwarzania danych.
