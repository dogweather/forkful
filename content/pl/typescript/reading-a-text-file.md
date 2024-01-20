---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Czytanie pliku tekstowego to proces otwierania i odczytywania zawartości pliku używając kodu. Programiści robią to, aby przetworzyć najróżniejsze dane: konfiguracje, dane wejściowe, logi i wiele innych.

## Jak to zrobić:

W TypeScript możemy wykorzystać moduł 'fs' do odczytania pliku. Oto prosty przykład:

```TypeScript
import * as fs from 'fs';

fs.readFile('plik.txt', 'utf8', function(err, data) {
    if (err) {
        return console.error(err);
    }
    console.log(data); 
});
```
Jeżeli np. w pliku `plik.txt` mieliśmy tekst "Witaj, Świecie!", to wynikiem tego kodu powinno być:

```
Witaj, Świecie!
```

## W głębokie wody:

Historia: Czytanie plików istniało już od wczesnej ery informatyki, gdy jednym ze sposobów przechowywania danych były taśmy magnetyczne.

Alternatywy: Inne sposoby odczytu pliku to m.in: sync read, read streams. Wybór zależy od konkretnej sytuacji.

Szczegóły implementacji: Metoda `readFile` wczytuje cały plik do pamięci przed wywołaniem callback, co może być problematyczne dla dużych plików. Aby to ominąć, warto skorzystać z read stream.

## Zobacz też:

- [Dokumentacja fs Node.js](https://nodejs.org/api/fs.html)

Pamiętaj, że TypeScript jest tylko nakładką na JavaScript, więc wiele informacji na tematy JavaScript również będzie przydatne!