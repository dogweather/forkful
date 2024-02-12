---
title:                "Wyszukiwanie i zamiana tekstu"
aliases:
- /pl/google-apps-script/searching-and-replacing-text.md
date:                  2024-02-01T22:01:26.662818-07:00
model:                 gpt-4-0125-preview
simple_title:         "Wyszukiwanie i zamiana tekstu"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/google-apps-script/searching-and-replacing-text.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyszukiwanie i zastępowanie tekstu w Google Apps Script polega na programowym identyfikowaniu konkretnych ciągów znaków w dokumencie, arkuszu kalkulacyjnym lub innym typie zawartości Google Apps i zastępowaniu ich innymi wartościami tekstowymi. Programiści wykorzystują tę funkcjonalność do automatyzowania edycji dużych ilości treści, korygowania powszechnych błędów, ujednolicania terminologii w dokumentach lub wstawiania dynamicznych danych do szablonów.

## Jak to zrobić:

Google Apps Script oferuje prosty sposób na wyszukiwanie i zastępowanie tekstu, zwłaszcza w Google Docs i Arkuszach. Poniżej przykłady dla obu.

### Google Docs:

Aby wyszukać i zastąpić tekst w dokumencie Google, głównie będziesz korzystać z klasy `DocumentApp`.

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // Aby wyszukać i zastąpić konkretną frazę
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// Użycie
searchReplaceInDoc();
```

Ten fragment kodu wyszukuje wszystkie wystąpienia `'searchText'` w aktywnym dokumencie Google i zastępuje je `'replacementText'`.

### Google Sheets:

Podobnie, w Arkuszach Google, można użyć `SpreadsheetApp` do przeprowadzenia operacji wyszukiwania i zastępowania:

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // Wyszukaj i zastąp w aktualnie aktywnym arkuszu
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// Użycie
searchReplaceInSheet();
```

W tym przykładzie, `createTextFinder('searchText')` wyszukuje w aktywnym arkuszu 'searchText', a `replaceAllWith('replacementText')` zastępuje wszystkie wystąpienia 'replacementText'.

## Szczegółowe rozważania

Funkcjonalność wyszukiwania i zastępowania w Google Apps Script jest mocno związana z jego internetową naturą, co pozwala skryptom na płynną manipulację tekstem w różnych aplikacjach Google. Historycznie, ta możliwość wynika z szerszego kontekstu przetwarzania i manipulacji tekstem w programowaniu, gdzie regularne wyrażenia i funkcje ciągów w językach takich jak Perl i Python ustanowiły wysoki standard elastyczności i mocy.

Chociaż funkcjonalność wyszukiwania i zastępowania w Google Apps Script jest potężna do prostych substytucji, brakuje jej pełnych możliwości regularnych wyrażeń znajdowanych w niektórych innych językach. Na przykład, chociaż można używać podstawowych regularnych wyrażeń w `createTextFinder` w Arkuszach Google, opcje dla skomplikowanego dopasowywania wzorców i manipulacji są ograniczone w porównaniu do Perla czy Pythona.

Dla bardziej zaawansowanych potrzeb przetwarzania tekstu programiści mogą zwrócić się ku eksporcie zawartości Google Docs lub Arkuszy do formatu, który może być przetworzony zewnętrznie z użyciem potężniejszych języków, lub wykorzystując Google Apps Script do wywoływania zewnętrznych API lub usług oferujących bardziej zaawansowane możliwości manipulacji tekstem.

Pomimo tych ograniczeń, dla większości typowych zadań wyszukiwania i zastępowania w ekosystemie aplikacji Google, Google Apps Script oferuje prosty, efektywny i wysoko zintegrowany rozwiązanie dostosowane do potrzeb automatyzacji i skryptowania w ramach pakietu narzędzi produktywnościowych Google.
