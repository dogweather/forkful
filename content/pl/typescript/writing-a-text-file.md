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

## Dlaczego

Pisanie plików tekstowych jest nieodłączną częścią procesu programowania. Niezależnie od tego, czy jesteś doświadczonym programistą czy dopiero zaczynasz swoją przygodę z językiem TypeScript, znajomość pisania plików tekstowych jest niezbędnym umiejętnością.

## Jak to zrobić

W celu napisania pliku tekstowego przy użyciu języka TypeScript, skorzystaj z funkcji `writeFile` z biblioteki `fs`. Poniżej znajduje się przykładowy kod, który wyjaśni krok po kroku, jak napisać plik tekstowy:

```TypeScript
// Importowanie biblioteki fs
import * as fs from 'fs';

// Utworzenie funkcji, która będzie zapisywać plik
function writeToFile(filename: string, data: string) {
    // Wywołanie funkcji writeFile z podaniem nazwy pliku oraz danych do zapisania
    fs.writeFile(filename, data, (err: Error) => {
        // W przypadku błędu, wyświetl komunikat
        if (err) {
            console.log(err);
        } else {
            console.log('Plik został zapisany.');
        }
    });
}

// Wywołanie funkcji z podaniem nazwy pliku oraz danych do zapisania
writeToFile('moj_plik.txt', 'To jest moj pierwszy plik tekstowy!');

```

Running the above code will write the specified data to a text file named "moj_plik.txt". To verify that the file was written successfully, you can navigate to the file location and open it to see the text inside.

## Zagłębienie się w temat

Tworzenie plików tekstowych jest jedną z podstawowych form komunikacji w programowaniu. Może być używane do przechowywania danych, jak również wyświetlania informacji dla użytkownika. Oprócz funkcji `writeFile`, biblioteka `fs` udostępnia również inne metody, takie jak `readFile` i `appendFile`, które pozwalają na odczytywanie i dopisywanie treści do istniejących plików tekstowych. Pozwala to na jeszcze większą kontrolę nad danymi przechowywanymi w plikach.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o pisaniu plików tekstowych w języku TypeScript, możesz zapoznać się z dokumentacją oficjalnej biblioteki `fs` na stronie [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html). Możesz również zobaczyć przykładowy projekt na GitHubie, który wykorzystuje funkcje `fs` do tworzenia pliku tekstowego [https://github.com/username/repo](https://github.com/username/repo).