---
title:                "Tworzenie pliku tekstowego"
html_title:           "Javascript: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie plików tekstowych jest podstawową czynnością w programowaniu. Polega ono na zapisywaniu tekstu na dysku komputera w postaci pliku. Programiści robią to, aby przechowywać i przetwarzać informacje w ich programach.

## Jak to zrobić:

Aby zapisać plik tekstowy za pomocą Javascript, musimy użyć wbudowanej funkcji "fs.writeFile()". Przykładowy kod wygląda następująco:

```Javascript
const fs = require('fs'); //importujemy moduł do manipulacji plikami

let data = "To jest przykładowy tekst do zapisania w pliku."; //definiujemy tekst do zapisania

fs.writeFile('plik.txt', data, (err) => { //tworzymy plik o nazwie "plik.txt" i wpisujemy do niego tekst
  if (err) throw err;
  console.log('Plik został zapisany!'); //wyświetlamy informację o zapisaniu pliku
});
```

Po wykonaniu tego kodu, na naszym dysku zostanie utworzony plik "plik.txt", w którym znajdzie się tekst zdefiniowany w zmiennej "data".

## Zanurz się głębiej:

Pisanie plików tekstowych w Javascript jest możliwe dzięki modułowi "fs", który został wprowadzony w wersji 0.1.8 tej technologii. Istnieje również wiele alternatywnych metod zapisu plików tekstowych, takich jak na przykład używanie aplikacji zewnętrznych lub wykorzystanie innych języków programowania. Ważne jest, aby pamiętać o odpowiednim formatowaniu danych, aby plik był czytelny dla ludzi oraz dla programów.

## Zobacz także:

- Dokumentacja funkcji fs.writeFile(): https://nodejs.org/api/fs.html#fs_fs_writefile_file_data_options_callback
- Przydatny poradnik o manipulacji plikami w Javascript: https://www.w3schools.com/nodejs/nodejs_filesystem.asp
- Alternatywne sposoby zapisu plików tekstowych: https://blog.fullstacktraining.com/ways-to-write-to-file-in-nodejs/