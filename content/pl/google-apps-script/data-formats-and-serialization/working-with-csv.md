---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:11.168082-07:00
description: "Jak to zrobi\u0107: Aby odczyta\u0107 dane CSV z pliku przechowywanego\
  \ w Google Drive, najpierw musisz pobra\u0107 zawarto\u015B\u0107 pliku jako ci\u0105\
  g znak\xF3w, a nast\u0119pnie j\u0105\u2026"
lastmod: '2024-03-13T22:44:34.927919-06:00'
model: gpt-4-0125-preview
summary: "Aby odczyta\u0107 dane CSV z pliku przechowywanego w Google Drive, najpierw\
  \ musisz pobra\u0107 zawarto\u015B\u0107 pliku jako ci\u0105g znak\xF3w, a nast\u0119\
  pnie j\u0105 przeanalizowa\u0107."
title: Praca z formatem CSV
weight: 37
---

## Jak to zrobić:


### Odczytywanie danych CSV
Aby odczytać dane CSV z pliku przechowywanego w Google Drive, najpierw musisz pobrać zawartość pliku jako ciąg znaków, a następnie ją przeanalizować. Google Apps Script ułatwia pobieranie zawartości pliku dzięki usłudze DriveApp.

```javascript
function readCSV() {
  var fileId = 'TWOJE_ID_PLIKU_TUTAJ'; // Zastąp rzeczywistym ID pliku
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // Rejestrowanie komórek każdego wiersza
  }
}
```

### Zapisywanie danych CSV
Tworzenie i zapisywanie do pliku CSV wymaga skonstruowania ciągu znaków z wartościami rozdzielonymi przecinkami i nowymi liniami, a następnie zapisania lub wyeksportowania go. Ten przykład demonstruje tworzenie nowego pliku CSV w Google Drive.

```javascript
function writeCSV() {
  var folderId = 'TWOJE_ID_FOLDERU_TUTAJ'; // Zastąp ID folderu na Dysku, w którym zostanie utworzony nowy plik
  var csvContent = "Name,Age,Occupation\nJohn Doe,29,Engineer\nJane Smith,34,Designer";
  var fileName = "przyklad.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### Przykładowe wyniki
Podczas rejestrowania komórek wierszy podczas odczytywania CSV:

```plaintext
[John, 29, Engineer]
[Jane, 34, Designer]
```

Podczas zapisywania, tworzony jest plik o nazwie "przyklad.csv" z zawartością:

```plaintext
Name,Age,Occupation
John Doe,29,Engineer
Jane Smith,34,Designer
```

## Wnikliwe spojrzenie
Historycznie, pliki CSV były cenione za ich prostotę i czytelność dla człowieka, co czyniło je dostępnymi dla nieprogramistów i użytecznymi do szybkiego przeglądania danych. Jednakże Google Apps Script działa w ramach ekosystemu Google, gdzie Google Sheets działa jako potężna, przyjazna dla użytkownika alternatywa dla manipulacji plikami CSV. Arkusze nie tylko zapewniają GUI do edytowania danych, ale także obsługują skomplikowane formuły, style i wiele innych funkcji, których brak w surowych plikach CSV.

Pomimo zalet oferowanych przez Google Sheets, bezpośrednia manipulacja plikami CSV w Google Apps Script pozostaje ważna dla zautomatyzowanych zadań, szczególnie przy pracy z zewnętrznymi systemami generującymi lub wymagającymi danych w formacie CSV. Na przykład, przy integracji z systemami starszego typu, eksportowaniu danych do użytku w innych aplikacjach lub przetwarzaniu wstępnym przed wprowadzeniem danych do Google Sheets.

Ponadto, możliwości Google Apps Script w pracy z plikami CSV mogą być rozszerzone dzięki usłudze Utilities dla zaawansowanych potrzeb kodowania, lub zinterfejsowane z zewnętrznymi API do konwersji, analizy lub walidacji zadań. Jednakże, przy pracy z dużymi zbiorami danych lub wymagających skomplikowanych manipulacji, rozważ użycie Google Sheets API lub eksplorację BigQuery dla bardziej zaawansowanych możliwości przetwarzania danych.

Chociaż prostota pozostaje kluczowym powodem popularności CSV, te alternatywy oferują bogatszy zestaw funkcji do pracy z danymi w obszernym ekosystemie Google Cloud.
