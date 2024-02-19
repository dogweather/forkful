---
aliases:
- /pl/google-apps-script/creating-a-temporary-file/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:01.783838-07:00
description: "Tworzenie tymczasowego pliku w Google Apps Script polega na generowaniu\
  \ pliku przeznaczonego do kr\xF3tkotrwa\u0142ego u\u017Cytku, zazwyczaj do przetwarzania\
  \ danych\u2026"
lastmod: 2024-02-18 23:08:49.173923
model: gpt-4-0125-preview
summary: "Tworzenie tymczasowego pliku w Google Apps Script polega na generowaniu\
  \ pliku przeznaczonego do kr\xF3tkotrwa\u0142ego u\u017Cytku, zazwyczaj do przetwarzania\
  \ danych\u2026"
title: Tworzenie tymczasowego pliku
---

{{< edit_this_page >}}

## Co i Dlaczego?

Tworzenie tymczasowego pliku w Google Apps Script polega na generowaniu pliku przeznaczonego do krótkotrwałego użytku, zazwyczaj do przetwarzania danych pośrednich, debugowania lub w celach cache’owania. Programiści robią to, aby zarządzać danymi tymczasowo, bez zaśmiecania stałej przestrzeni do przechowywania, lub gdy trwałość danych nie jest potrzebna poza zakresem bieżącego procesu.

## Jak to zrobić:

W Google Apps Script, stworzenie tymczasowego pliku można osiągnąć za pomocą serwisu DriveApp, który zapewnia prostą metodę na tworzenie, czytanie i usuwanie plików w Google Drive. Oto jak możesz stworzyć tymczasowy plik tekstowy, zapisać do niego jakieś dane, a następnie usunąć go po użyciu:

```javascript
function createTemporaryFile() {
  // Stworzenie tymczasowego pliku o nazwie "tempFile.txt"
  var tempFile = DriveApp.createFile('tempFile.txt', 'Tymczasowa zawartość', MimeType.PLAIN_TEXT);
  
  // Logowanie URL pliku dla dostępu lub debugowania
  Logger.log('Tymczasowy plik stworzony: ' + tempFile.getUrl());
  
  // Przykładowa operacja: Odczytanie zawartości pliku
  var content = tempFile.getBlob().getDataAsString();
  Logger.log('Zawartość tempFile: ' + content);
  
  // Zakładając, że operacja została zakończona i plik nie jest już potrzebny
  // Usunięcie tymczasowego pliku
  tempFile.setTrashed(true);
  
  // Potwierdzenie usunięcia
  Logger.log('Tymczasowy plik usunięty');
}
```

Uruchomienie tego skryptu spowoduje wyświetlenie:

```
Tymczasowy plik stworzony: [URL stworzonego tymczasowego pliku]
Zawartość tempFile: Tymczasowa zawartość
Tymczasowy plik usunięty
```

Ten przykładowy skrypt prezentuje stworzenie tymczasowego pliku, wykonanie operacji odczytu jego zawartości oraz w końcu usunięcie pliku w celu oczyszczenia.

## Dogłębna analiza

Koncepcja tworzenia tymczasowych plików w rozwoju oprogramowania jest równie stara, jak koncepcja zarządzania plikami. W tradycyjnych systemach plików, tymczasowe pliki często są tworzone w wyznaczonych katalogach temp i są kluczowe dla różnych procesów pośrednich, takich jak sortowanie dużych zbiorów danych, przechowywanie danych sesji dla aplikacji internetowych lub przechowywanie fragmentów danych podczas procesów konwersji plików.

W Google Apps Script, proces tworzenia tymczasowych plików wykorzystuje infrastrukturę Google Drive, co oferuje interesującą mieszankę zarządzania plikami w chmurze z tradycyjnymi koncepcjami programowania. Jednakże, ta metoda tworzenia tymczasowych plików w Google Drive nie jest pozbawiona swoich ograniczeń i kosztów, biorąc pod uwagę ograniczenia kwot Google Drive. Również opóźnienia w dostępie do Google Drive przez sieć w porównaniu z lokalnym systemem plików mogą być kluczowym czynnikiem dla aplikacji wysokiej wydajności.

Jako alternatywy, deweloperzy mogą rozważyć użycie Google Sheets dla małych zbiorów danych, które wymagają tymczasowego przechowywania podczas obliczeń, lub Google Cloud Storage dla aplikacji wymagających wysokiej wydajności operacji odczytu/zapisu i większych pojemności przechowywania. Każde z tych rozwiązań oferuje inne kompromisy dotyczące opóźnień, limitów przechowywania i łatwości użycia z Google Apps Script. Ostatecznie, wybór zależy od konkretnych wymagań aplikacji oraz istniejącej infrastruktury, w ramach której działa.
