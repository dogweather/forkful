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

## Dlaczego

Pisanie plików tekstowych to często wymagana umiejętność w programowaniu. Zapisywanie informacji w postaci tekstu jest nie tylko wygodne, ale także powszechnie używane w różnych dziedzinach.

## Jak to zrobić

Pisanie plików tekstowych w języku JavaScript jest proste i prostydłaczego mieć kilka przykładowych kodów oraz ich wyjścia. Kod będzie umieszczony w blokach "```javascript ... ```", wystarczy skopiować go do swojego edytora kodu i uruchomić, aby zobaczyć efekt.

```javascript
// Tworzenie pliku tekstowego i zapisanie w nim tekstu
const fs = require('fs'); // importowanie modułu fs

const tekst = 'To jest tekstowy plik, który zostanie zapisany.'; 
// deklaracja zmiennej z tekstem do zapisania

fs.writeFile('plik.txt', tekst, (err) => {
  if (err) throw err; // obsługa błędu
  console.log('Plik został zapisany.');
});
// wywołanie metody writeFile i przekazanie nazwy pliku, tekstu oraz funkcji zwrotnej

// Odczytywanie tekstu z pliku
fs.readFile('plik.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data); // wyświetlenie odczytanego tekstu
});
```

Przykładowe wyjście:

```
Plik został zapisany.
To jest tekstowy plik, który zostanie zapisany.
```

## Głębsza analiza

Operacje na plikach tekstowych w języku JavaScript są możliwe dzięki modułowi fs (File System). Jest to wbudowany moduł, więc nie ma potrzeby instalacji zewnętrznych bibliotek.

Metoda `writeFile` służy do zapisywania tekstu w pliku o określonej nazwie. Przyjmuje trzy argumenty: nazwę pliku, tekst do zapisania oraz funkcję zwrotną, która zostanie wywołana po zapisaniu pliku. W przypadku wystąpienia błędu, wywołana zostanie funkcja `throw` i wyświetlony zostanie komunikat o błędzie.

Natomiast metoda `readFile` służy do odczytywania tekstu z pliku, również przyjmując trzy argumenty: nazwę pliku, kodowanie tekstu (w tym przypadku `utf8` oznacza, że tekst będzie odczytywany w formacie UTF-8) oraz funkcję zwrotną. W przypadku braku błędów, odczytany tekst zostanie przekazany do wywołania funkcji zwrotnej.

Jeśli chcesz dowiedzieć się więcej o operacjach na plikach tekstowych w języku JavaScript, zapoznaj się z dokumentacją modułu fs lub poszukaj większej ilości przykładów kodów.

## Zobacz także

- Dokumentacja modułu fs: https://nodejs.org/api/fs.html
- Przykładowe operacje na plikach tekstowych w języku JavaScript: https://www.techiediaries.com/nodejs-tutorial-read-write-files/