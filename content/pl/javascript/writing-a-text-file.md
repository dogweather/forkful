---
title:                "Javascript: Pisanie pliku tekstowego"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest ważnym aspektem programowania w języku Javascript. Dzięki nim możemy przechowywać i odczytywać dane w formie tekstowej, co jest niezbędne w wielu aplikacjach. Jest to również przydatne w przypadku, gdy chcemy udostępnić lub dzielić się danymi z innymi użytkownikami.

## Jak To Zrobić

Aby napisać plik tekstowy w języku Javascript, musimy wykorzystać wbudowane w ten język funkcje do obsługi plików. Najpierw musimy otworzyć lub stworzyć nowy plik tekstowy przy użyciu metody `fs.open()` i przekazać mu nazwę i tryb dostępu do pliku. Następnie możemy użyć metody `fs.write()` do zapisania danych do pliku. Przykładowy kod wyglądałby następująco:

```Javascript
const fs = require('fs');

// Otwieramy plik "hello.txt" w trybie "w" (zapis)
fs.open('hello.txt', 'w', (err, file) => {
  if (err) throw err;
  // Używamy metody write() aby zapisać dane do pliku
  fs.write(file, 'Witaj, świecie!', (err) => {
    if (err) throw err;
    console.log('Plik został zapisany.');
  });
});
```
Po uruchomieniu tego kodu, plik "hello.txt" powinien zostać stworzony w katalogu, w którym jest uruchamiany skrypt, z zawartością "Witaj, świecie!". Możemy także użyć metody `fs.append()` aby dopisać dane do istniejącego pliku tekstowego.

## Głębszy Wgląd

Powyższy sposób jest jednym z najprostszych sposobów na zapisywanie danych do pliku tekstowego w języku Javascript. Istnieje jednak wiele innych metod do obsługi plików, takich jak `fs.writeFile()` czy `fs.writeFileSync()`. Warto także pamiętać o odpowiednim obsłużeniu błędów oraz zamykaniu plików po użyciu.

## Zobacz także

- Dokumentacja Node.js na temat obsługi plików: https://nodejs.org/api/fs.html
- Przykładowy projekt wykorzystujący możliwości pisania plików w języku Javascript: https://github.com/jjcapellan/save-text-file-in-nodejs