---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:50:33.094944-07:00
description: "Sprawdzanie, czy katalog istnieje w Google Apps Script, polega na weryfikacji\
  \ obecno\u015Bci folderu w Google Drive. Programi\u015Bci cz\u0119sto wykonuj\u0105\
  \ to\u2026"
lastmod: '2024-03-13T22:44:34.919188-06:00'
model: gpt-4-0125-preview
summary: "Sprawdzanie, czy katalog istnieje w Google Apps Script, polega na weryfikacji\
  \ obecno\u015Bci folderu w Google Drive. Programi\u015Bci cz\u0119sto wykonuj\u0105\
  \ to\u2026"
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

## Co i dlaczego?

Sprawdzanie, czy katalog istnieje w Google Apps Script, polega na weryfikacji obecności folderu w Google Drive. Programiści często wykonują to sprawdzenie, aby uniknąć błędów lub redundancji tworzenia folderów podczas programowego zarządzania plikami i katalogami.

## Jak to zrobić:

Google Apps Script nie oferuje bezpośredniej metody "exists" dla folderów. Zamiast tego, używamy możliwości wyszukiwania Google Drive, aby sprawdzić, czy folder o określonej nazwie istnieje. Oto przykład krok po kroku:

```javascript
// Funkcja do sprawdzania, czy katalog istnieje
function checkIfDirectoryExists(directoryName) {
  // Pobierz kolekcję folderów odpowiadających określonej nazwie
  var folders = DriveApp.getFoldersByName(directoryName);
  
  // Sprawdź, czy istnieje co najmniej jeden folder o określonej nazwie
  if (folders.hasNext()) {
    Logger.log('Katalog istnieje.');
    return true;
  } else {
    Logger.log('Katalog nie istnieje.');
    return false;
  }
}

// Przykładowe użycie
var directoryName = 'Mój Przykładowy Folder';
checkIfDirectoryExists(directoryName);
```

Przykładowe wyjście:
```
Katalog istnieje.
```
lub 
```
Katalog nie istnieje.
```

Ten skrypt wykorzystuje metodę `getFoldersByName`, która pobiera wszystkie foldery w Drive użytkownika, które pasują do określonej nazwy. Ponieważ nazwy nie są unikalne w Drive, ta metoda zwraca `FolderIterator`. Obecność następnego elementu (`hasNext()`) w tym iteratorze wskazuje, że katalog istnieje.

## Szczegółowa analiza

Historycznie, zarządzanie plikami w środowiskach webowych i chmurowych ewoluowało znacząco. Google Apps Script, oferując obszerny API dla Google Drive, umożliwia skomplikowane operacje zarządzania plikami i folderami, w tym mechanizmy wyszukiwania i sprawdzania, które zostały zademonstrowane. Jednakże, godne uwagi jest brak bezpośredniego sprawdzenia istnienia, co prawdopodobnie wynika z dopuszczenia przez Google Drive wielokrotności folderów o tej samej nazwie, co kontrastuje z wieloma systemami plików egzekwującymi unikalność nazw w obrębie tego samego katalogu.

W tym kontekście, używanie metody `getFoldersByName` jest skutecznym obejściem, ale może potencjalnie wprowadzić nieefektywności w scenariuszu, gdzie istnieje duża liczba folderów z zduplikowanymi nazwami. Alternatywnym podejściem może być utrzymanie aplikacyjnej indeksacji specyficznej lub konwencji nazewnictwa, aby zapewnić szybsze sprawdzanie, szczególnie gdy wydajność staje się krytycznym problemem.

Chociaż podejście Google Apps Script może początkowo wydawać się mniej bezpośrednie w porównaniu do sprawdzania istnienia plików w językach programowania bezpośrednio łączących się z pojedynczym systemem plików, odzwierciedla to konieczność radzenia sobie ze złożonościami przechowywania plików w chmurze. Deweloperzy wykorzystujący Google Apps Script do zarządzania Drive powinni uwzględnić te niuanse, optymalizując pod kątem mocnych i słabych stron Google Drive.
