---
title:                "Odczytywanie pliku tekstowego"
html_title:           "TypeScript: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub uczysz się programowania, prawdopodobnie spotykasz się z różnymi rodzajami plików, w tym także plikami tekstowymi. Czy kiedykolwiek zastanawiałeś się, jak odczytać zawartość pliku tekstowego w swoim kodzie? W tym artykule dowiesz się, dlaczego warto nauczyć się tej umiejętności i jak to zrobić w TypeScript.

## Jak to zrobić

Oto kilka przykładów kodu, które pokazują, jak odczytywać pliki tekstowe w TypeScript:

```TypeScript
import fs from "fs";

// Funkcja asynchroniczna do odczytywania pliku tekstowego
async function readTextFile(filename: string) {
  try {
    // Wykorzystanie fs.promises.readFile() do odczytania pliku
    const data = await fs.promises.readFile(filename, "utf8");
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Wywołanie funkcji z nazwą pliku
readTextFile("tekstowy.txt");
```

Po uruchomieniu tego kodu, powinieneś zobaczyć zawartość pliku tekstowego "tekstowy.txt" w konsoli.

Jeśli chcesz odczytać plik synchronicznie, użyj metody fs.readFileSync(). Pamiętaj jednak, że ta metoda blokuje wykonanie kodu aż do momentu odczytania całego pliku, więc lepiej jest używać jej tylko w prostych scenariuszach.

Jeśli chcesz odczytać plik z danych binarnych, możesz użyć metody fs.readFile() z parametrem "utf8" zmienionym jako "binary". Możesz także użyć opcji "encoding" z wartością "binary".

## Deep Dive

Metoda fs.promises.readFile() zwraca obiekt typu Promise, który możesz obsłużyć przy użyciu instrukcji async/await lub metody .then(). Po zwróceniu zawartości pliku, możesz przetworzyć ją według własnych potrzeb i wykorzystać w swoim kodzie.

Pamiętaj także, że możesz manipulować plikami tekstowymi w TypeScript za pomocą wielu innych metod z modułu fs, takich jak tworzenie, usuwanie czy zmiana nazwy pliku.

Jeśli potrzebujesz więcej informacji na temat operacji na plikach w TypeScript, możesz przejrzeć dokumentację na stronie https://nodejs.org/api/fs.html.

## Zobacz także

- https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-8.html - informacje o nowych funkcjach w zakresie operacji plikowych w TypeScript
- https://www.digitalocean.com/community/tutorials/nodejs-reading-writing-files - przykłady użycia metod do operacji na plikach w Node.js
- https://www.typescriptlang.org/docs - oficjalna dokumentacja TypeScript