---
title:                "Javascript: Odczytywanie pliku tekstowego"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś zainteresowany programowaniem w języku Javascript, i chcesz nauczyć się czytać pliki tekstowe, ten post jest dla Ciebie!

## Jak to zrobić

Aby odczytać plik tekstowy w języku Javascript, musisz użyć wbudowanego modułu fs (FileSystem). Najpierw należy go zaimportować za pomocą polecenia `require`, a następnie użyć metody `readFileSync` w celu odczytania pliku. Przykład kodu znajduje się poniżej:

```Javascript
const fs = require('fs');

// Utwórz zmienną przechowującą ścieżkę do pliku
const filePath = 'tekst.txt';

// Użyj metody readFileSync, aby odczytać zawartość pliku
const fileContent = fs.readFileSync(filePath, 'utf-8');

// Wyświetl zawartość pliku w konsoli
console.log(fileContent);
```

Powyższy kod najpierw importuje moduł fs, następnie wykorzystuje metodę `readFileSync` do odczytania pliku tekstowego o nazwie "tekst.txt". Warto zauważyć, że jako drugi parametr przekazujemy kodowanie, którego chcemy użyć, w tym przypadku jest to `utf-8`. Warto też pamiętać, że metoda ta zwraca zawartość pliku jako string. 

Teraz, gdy już mamy odczytaną zawartość pliku, możemy wykonać jakiekolwiek operacje na tej zmiennej, np. wyświetlić ją na stronie internetowej lub zapisać do innego pliku. Poniżej znajduje się przykład kodu, który zapisze odczytaną zawartość do pliku `nowy_plik.txt`:

```Javascript
const fs = require('fs');

const filePath = 'tekst.txt';
const fileContent = fs.readFileSync(filePath, 'utf-8');

// Zapisz odczytaną zawartość do nowego pliku
fs.writeFileSync('nowy_plik.txt', fileContent);
```

W powyższym przykładzie wykorzystaliśmy metodę `writeFileSync`, która jako pierwszy parametr przyjmuje nazwę nowego pliku, do którego zapiszemy zawartość, a jako drugi parametr - zmienną z zawartością pliku, którą odczytaliśmy wcześniej. 

## Deep Dive

W powyższych przykładach omówiliśmy wyłącznie podstawy odczytywania plików tekstowych w języku Javascript. Warto jednak pamiętać, że istnieje wiele innych metod i opcji, które można wykorzystać, np. ustawianie kodowania, odczytywanie plików asynchronicznie czy wykorzystanie bufora. W celu pogłębienia wiedzy na ten temat, polecamy zapoznanie się z dokumentacją oficjalnego modułu fs oraz eksperymentowanie z różnymi opcjami.

## Zobacz również

- [Dokumentacja modułu fs w języku Javascript](https://nodejs.org/api/fs.html)
- [Tutorial odczytywania plików w języku Javascript](https://www.digitalocean.com/community/tutorials/how-to-read-a-file-with-node-js)
- [Kurs programowania w języku Javascript](https://www.codecademy.com/learn/introduction-to-javascript)