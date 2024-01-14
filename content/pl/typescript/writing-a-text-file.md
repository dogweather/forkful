---
title:    "TypeScript: Tworzenie pliku tekstowego"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest nieodłączną częścią pracy każdego programisty. Dzięki nim możemy przechowywać i przetwarzać różnego rodzaju dane w prosty i czytelny sposób. W tym artykule dowiesz się, jak pisać pliki tekstowe w języku TypeScript, aby ułatwić sobie pracę w przyszłości.

## Jak to zrobić

Pisanie plików tekstowych w języku TypeScript jest bardzo proste. Wystarczy użyć funkcji wbudowanej `writeFile()` z modułu `fs`. Przykładowy kod wyglądałby następująco:

```TypeScript
import fs from 'fs';

const data = "To jest przykładowy tekst, który zostanie zapisany w pliku.";

fs.writeFile('moj-plik.txt', data, (err) => {
  if (err) {
    console.error(err);
  }
  console.log("Plik został zapisany pomyślnie.");
});
```

Powyższy kod tworzy nowy plik tekstowy o nazwie `moj-plik.txt` i zapisuje w nim podany tekst. W przypadku wystąpienia błędu, zostanie on wyświetlony w konsoli, a w przeciwnym razie zostanie wyświetlone potwierdzenie zapisania pliku.

## Głębszy wgląd

Warto zauważyć, że funkcja `writeFile()` nie zwraca wartości, a jedynie wywołuje funkcję zwrotną po zapisaniu pliku. Dzięki temu możemy wykonywać inne operacje dopiero po pomyślnym zapisaniu pliku.

Ponadto, możemy również wykorzystać funkcję `appendFile()` z tego samego modułu, aby dopisywać dane do istniejącego pliku, zamiast go nadpisywać. Wystarczy zmienić `writeFile()` na `appendFile()` w przykładowym kodzie.

## Zobacz także

- [Dokumentacja funkcji writeFile()](https://nodejs.org/api/fs.html#fs_fs_writefile_file_data_options_callback)
- [Dokumentacja funkcji appendFile()](https://nodejs.org/api/fs.html#fs_fs_appendfile_path_data_options_callback)
- [Dokumentacja modułu fs w Node.js](https://nodejs.org/api/fs.html)