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

## Co & Dlaczego?

Czytasz ten artykuł, więc prawdopodobnie jesteś programistą lub chcesz nim zostać. W tym przewodniku wyjaśnię, czym jest odczytywanie pliku tekstowego oraz dlaczego jest to ważna umiejętność dla programistów.

Odczytywanie pliku tekstowego oznacza w praktyce otwarcie pliku, który zawiera tekst, i odczytanie go w sposób, który program może interpretować i wykorzystać. Dzięki temu twoja aplikacja może uzyskać dostęp do zewnętrznych danych, takich jak lista użytkowników lub konfiguracja.

Dlaczego więc programiści czytają pliki tekstowe? Jest to często wykorzystywane w celu konfiguracji lub odczytania danych, które są przechowywane w zewnętrznym źródle, takim jak baza danych. Pozwala to na łatwiejsze dostosowanie aplikacji do konkretnych potrzeb oraz ułatwia przechowywanie i dostęp do dużej ilości informacji.

## Jak to zrobić:

TypeScript jest językiem programowania, który umożliwia nam pracę z plikami tekstowymi w sposób prosty i intuicyjny. Przykładowy kod wyglądać może następująco:

```
const fs = require('fs'); // importujemy wbudowany moduł fs

fs.readFile('plik.txt', 'utf8', function(err, data) { //metoda readFile uruchamia funkcję zwrotną, która umożliwia odczytanie danych
    if (err) throw err;
    console.log(data); // wyświetlamy odczytane dane w konsoli
});
```

Przeanalizujmy teraz krok po kroku co dzieje się w tym kodzie:

- Najpierw importujemy wbudowany moduł fs, który pozwala na pracę z plikami.
- Następnie wykorzystujemy metodę readFile, która przyjmuje jako pierwszy argument ścieżkę do pliku, jako drugi opcje (w tym przykładzie 'utf8' oznacza, że chcemy odczytać plik jako tekst) oraz jako trzeci funkcję zwrotną, która zostanie wywołana po zakończeniu odczytu pliku.
- W tej funkcji zwrotnej sprawdzamy najpierw, czy wystąpił błąd (jeśli tak, wyrzucamy go), a następnie wyświetlamy odczytane dane w konsoli.

Według tych prostych kroków, możesz odczytywać pliki tekstowe w swojej aplikacji.

## Wnikliwa analiza:

Odczytywanie plików tekstowych jest jednym z podstawowych zadań, z którymi programiści mają do czynienia. Początki tego procesu sięgają czasów, gdy komputery były jeszcze maszynami wykorzystującymi kartki dziurkowane, a pliki zapisywane były w postaci zwykłych tekstu.

Alternatywą dla odczytywania plików tekstowych może być przechowywanie informacji w bazach danych lub wykorzystanie innego formatu, jak na przykład XML lub JSON. Jednakże, odczytywanie plików tekstowych jest jedną z najprostszych i najwygodniejszych metod, zwłaszcza gdy mamy do czynienia z prostymi informacjami, takimi jak tabele lub konfiguracje.

Do odczytywania plików tekstowych można wykorzystać również wbudowane metody w języku JavaScript, jednak TypeScript oferuje bardziej przejrzyste i zwięzłe rozwiązanie.

Jeśli jesteś zainteresowany dalszą nauką odczytywania plików tekstowych w TypeScript, polecam obejrzeć ten [film](https://www.youtube.com/watch?v=sxONpLdX4m8) oraz zapoznać się z dokumentacją wbudowanego modułu fs.

## Zobacz również:

[Tutorial odczytu plików w TypeScrpt](https://hackernoon.com/handling-file-upload-in-react-and-node-js-server-d8c076c9aede)

[Dokumentacja wbudowanego modułu fs](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)