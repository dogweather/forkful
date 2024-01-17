---
title:                "Tworzenie pliku tekstowego"
html_title:           "TypeScript: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

Cześć programiści! Dzisiaj zanurzymy się w świecie pisania plików tekstowych w języku TypeScript. Dowiesz się, czym jest pisanie plików tekstowych i dlaczego programiści to robią, a także zobaczysz przykładowy kod i dowiesz się nieco więcej o tym temacie. Gotowi?

## Co to jest i dlaczego?

Pisanie plików tekstowych jest procesem tworzenia i zapisywania zawartości w pliku, który może być czytany jako zwykły tekst przez programy komputerowe. Programiści często piszą pliki tekstowe, aby przechowywać dane lub instrukcje, które mogą być łatwo odczytane i przetworzone przez inne programy.

## Jak to zrobić:

```TypeScript
import fs from 'fs';

// Tworzenie nowego pliku tekstowego
fs.writeFileSync('mojplik.txt', 'To jest zawartość mojego pliku.');

// Odczytywanie pliku tekstowego
const zawartoscPliku = fs.readFileSync('mojplik.txt', 'utf-8');
console.log(zawartoscPliku); // To jest zawartość mojego pliku.

// Dodawanie nowych danych do pliku tekstowego
fs.appendFileSync('mojplik.txt', '\nTo jest nowa linia tekstu.');
```

W powyższym przykładzie używamy modułu `fs` (od ang. file system) do operacji związanych z plikami. `writeFileSync()` tworzy nowy plik o podanej nazwie i zawartej w drugim argumencie zawartości. `readFileSync()` czyta zawartość istniejącego pliku i zwraca go jako ciąg znaków, który możemy przypisać do zmiennej i wykorzystać w programie. Aby dodać nowe dane do pliku, używamy `appendFileSync()`, który dodaje podaną zawartość na końcu pliku.

## Głębszy zanurzenie:

### Kontekst historyczny
Pisanie plików tekstowych jest jednym z podstawowych sposobów komunikacji między programami od wielu lat. Pliki tekstowe są nadal powszechnie używane w dzisiejszych czasach, ale istnieje wiele innych sposobów przesyłania i przechowywania danych.

### Alternatywy
Istnieje wiele alternatywnych sposobów zapisywania danych w języku TypeScript, w szczególności z użyciem baz danych lub plików JSON. Jednak pisanie plików tekstowych jest nadal popularną opcją, ponieważ jest to prosty i czytelny sposób przechowywania danych.

### Szczegóły implementacji
Podstawowym sposobem pisania danych do pliku tekstowego jest użycie funkcji `writeFileSync()` lub `appendFileSync()`, ale istnieją także inne metody, takie jak strumienie lub wykorzystanie bibliotek zewnętrznych.

## Zobacz również:
- [Dokumentacja Node.js o module fs](https://nodejs.org/api/fs.html)
- [Inne sposoby przechowywania danych w TypeScript](https://www.telesurtv.net/english/analysis/4-Types-of-Data-Storage-in-TSCHS-20180519-0023.html)