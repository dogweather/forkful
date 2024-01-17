---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Javascript: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?:

Czy kiedykolwiek zastanawiałeś się, co to znaczy czytać pliki tekstowe w programowaniu? Jest to proces, w którym programista odczytuje zawartość pliku tekstowego, czyli pliku zapisanego w formie tekstu, bez formatowania lub struktury. Programiści często wykonują tę czynność, aby uzyskać dostęp do informacji zapisanych w takim pliku, np. do danych lub ustawień programu.

Dlaczego czytanie plików tekstowych jest ważne dla programistów? Ponieważ daje to możliwość interakcji z różnymi rodzajami danych, które mogą być użyteczne w ich projektach. Może to również pomóc w automatyzacji pewnych zadań, takich jak przetwarzanie danych lub wyświetlanie zawartości pliku dla użytkownika.

## Jak to zrobić?
Jeśli chcesz nauczyć się czytać pliki tekstowe w Javascripcie, to powinieneś zacząć od poznania kilku podstawowych funkcji i metod. Przykładem może być funkcja `readFile()` z modułu `fs`, która służy do odczytywania plików. Poniżej przedstawiony jest kod, który na przykładzie pliku `data.txt` odczytuje jego zawartość i wyświetla ją w konsoli:

```Javascript
const fs = require('fs');
fs.readFile('data.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

Wykorzystujemy tutaj `readFile()` do odczytania pliku `data.txt`, definiujemy kodowanie jako `utf8`, aby odczytać plik jako tekst, a callback funkcja wyświetla odczytaną zawartość w konsoli. Dodatkowo, musimy obsłużyć błąd, dlatego używamy warunku `if` i `throw` dla błędu.

## Głębszy zanurzenie
Czytanie plików tekstowych jest powszechnie stosowane w programowaniu, a jedną z przyczyn tego jest fakt, że pliki tekstowe są łatwe do zrozumienia przez komputery. Kodowanie ich jest proste i nie wymaga specjalistycznych narzędzi, co jest dużą zaletą. Innym powodem jest fakt, że pliki tekstowe są uniwersalne, co oznacza, że ​​mogą być czytane przez różne języki programowania.

Alternatywą dla funkcji `readFile()` jest metoda `readFileSync()`, która działa synchronicznie, co oznacza, że ​​kod zostanie wstrzymany, aż plik zostanie odczytany. Istnieją również inne narzędzia i biblioteki, takie jak `fse` lub `readline`, które mogą ułatwić czytanie plików tekstowych.

W celu lepszego zrozumienia czytania plików tekstowych w Javascripcie, warto zapoznać się z formatem JSON (JavaScript Object Notation), który jest często używany do przechowywania danych w plikach tekstowych.

## Zobacz również
Jeśli jesteś zainteresowany dalszą nauką na temat czytania plików tekstowych w Javascripcie, to warto przejrzeć poniższe źródła:

- [Dokumentacja JavaScript do odczytywania plików](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/fs/readFile)
- [Podstawy programowania: czytanie plików w Node.js](https://www.youtube.com/watch?v=6UFyHmQ_Gqk)
- [Dokumentacja formatu JSON](https://www.json.org/json-pl.html)