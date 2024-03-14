---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:25.678548-07:00
description: "Odczytywanie pliku tekstowego w Google Apps Script (GAS) polega na dost\u0119\
  pie do danych tekstowych z plik\xF3w przechowywanych w Google Drive lub innym\u2026"
lastmod: '2024-03-13T22:44:34.922496-06:00'
model: gpt-4-0125-preview
summary: "Odczytywanie pliku tekstowego w Google Apps Script (GAS) polega na dost\u0119\
  pie do danych tekstowych z plik\xF3w przechowywanych w Google Drive lub innym\u2026"
title: Czytanie pliku tekstowego
---

{{< edit_this_page >}}

## Co i dlaczego?

Odczytywanie pliku tekstowego w Google Apps Script (GAS) polega na dostępie do danych tekstowych z plików przechowywanych w Google Drive lub innym dostępnym chmurowym schowku. Programiści często potrzebują odczytywać te pliki, aby importować, manipulować lub analizować dane tekstowe bezpośrednio w swoich projektach GAS, umożliwiając automatyzację i integrację z pakietem produktów Google.

## Jak to zrobić:

Aby zacząć czytać plik tekstowy za pomocą Google Apps Script, zazwyczaj należy użyć Google Drive API. Oto podstawowy przykład pokazujący, jak odczytać plik z Google Drive:

```javascript
function readFileContents(fileId) {
  // Uzyskuje plik z Google Drive po ID
  var file = DriveApp.getFileById(fileId);
  
  // Pobiera dane blob jako tekst
  var text = file.getBlob().getDataAsString();
  
  // Rejestruje zawartość w logu Google Apps Script
  Logger.log(text);
  return text;
}
```

*Przykładowe wyjście w logu:*

```
Hello, world! This is a test text file.
```

W tym przykładzie `fileId` to unikatowy identyfikator pliku, który chcesz przeczytać. Usługa `DriveApp` pobiera plik, a `getDataAsString()` czyta jego zawartość jako ciąg. Następnie możesz manipulować tym tekstem lub używać go według potrzeb.

## Wnikliwa analiza

Historycznie, odczytywanie plików tekstowych w aplikacjach internetowych, takich jak te budowane z Google Apps Script, stanowiło wyzwanie ze względu na ograniczenia bezpieczeństwa przeglądarek i asynchroniczną naturę JavaScriptu. Google Apps Script upraszcza to dzięki swoim abstrakcyjnym usługom takim jak `DriveApp`, zapewniając wysokopoziomowe API do interakcji z plikami w Google Drive.

Jednak ważnym aspektem jest wydajność i ograniczenia czasu wykonania nałożone przez Google Apps Script, zwłaszcza podczas czytania dużych plików lub wykonywania złożonych operacji z danymi. W niektórych przypadkach może być bardziej efektywne bezpośrednie użycie usług Google Cloud z potężniejszego backendu lub przetworzenie plików na bardziej zarządzalne fragmenty.

Dla złożonej obróbki plików lub gdy krytyczna jest wydajność w czasie rzeczywistym, alternatywy takie jak Google Cloud Functions, które obsługują Node.js, Python i Go, mogą oferować większą elastyczność i zasoby obliczeniowe. Niemniej jednak, dla prostych zadań w ekosystemie Google, zwłaszcza tam, gdzie priorytetem jest prostota i łatwość integracji z produktami Google, Google Apps Script oferuje nadzwyczajnie przyjazne podejście.
