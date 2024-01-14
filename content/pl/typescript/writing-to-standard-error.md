---
title:                "TypeScript: Pisanie do standardowego wyjścia błędów"
simple_title:         "Pisanie do standardowego wyjścia błędów"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarzają się sytuacje, w których jako programista musisz poinformować użytkownika o wystąpieniu błędu lub wyświetlić pewne ważne informacje. W takim przypadku używana jest funkcja `console.log()`, która wypisuje tekst na standardowe wyjście. Jednak, co w sytuacji, gdy chcesz przekazać informacje do standardowego wyjścia błędów? W tym przypadku przyda nam się funkcja `process.stderr.write()`, która pozwala na wypisanie tekstu na standardowe wyjście błędów. W tym artykule dowiemy się dlaczego warto korzystać z tej funkcji oraz jak ją używać.

## Jak to zrobić

Aby wypisać tekst na standardowe wyjście błędów, musimy najpierw zaimportować moduł `process` z pakietu `@types/node`. Możemy to zrobić przy pomocy polecenia:

```TypeScript
import * as process from 'process'; 
```

Następnie, korzystając z funkcji `process.stderr.write()`, możemy przekazać tekst, który chcemy wypisać na standardowe wyjście błędów. Przykładowy kod może wyglądać następująco:

```TypeScript
process.stderr.write('Błąd: Nieprawidłowy format danych'); 
```

Po wykonaniu takiego kodu, powyższy tekst zostanie wypisany na standardowym wyjściu błędów:

```
Błąd: Nieprawidłowy format danych
```

## Deep Dive

Funkcja `process.stderr.write()` jest często używana w celu poinformowania użytkownika o błędzie w programie. Jednak warto również wiedzieć, że można również przekierować standardowe wyjście błędów do pliku, korzystając z operatora `>` w terminalu. Dzięki temu można łatwo zapisywać informacje o błędach do pliku, co może być przydatne w przypadku późniejszej analizy i debugowania programu.

## Zobacz też

* [Dokumentacja funkcji `process.stderr.write()` (w języku angielskim)](https://nodejs.org/api/process.html#process_process_stderr_write_data_encoding_callback)
* [Artykuł o standardowym wyjściu i wyjściu błędów w Node.js (w języku polskim)](https://poradnikjavascript.pl/standardowe-wyjscie-i-wyjscie-bledow-w-node-js/)

Dziękuję za przeczytanie tego artykułu. Mam nadzieję, że pomógł on w zrozumieniu funkcji `process.stderr.write()` oraz w jej wykorzystaniu w Twoich projektach. Jeśli masz jakieś pytania lub uwagi, daj znać w komentarzu poniżej. Do zobaczenia w kolejnych artykułach!