---
title:                "Pisanie do standardowego błędu"
html_title:           "Ruby: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Co i Po co?

Pisanie na standardowe wyjście błędu jest jednym ze sposobów wyświetlania informacji o błędach lub ostrzeżeń dla użytkownika. Programiści zwykle używają tej metody, aby wyświetlić ważne informacje o swoim kodzie lub aby naprawić błędy w szybki i efektywny sposób.

Jak to zrobić:
```Ruby
puts "Hello World!" # wyświetla na standardowym wyjściu
$stderr.puts "Oops, something went wrong!" # wyświetla na standardowym wyjściu błędu
```
Output:
```
Hello World!
Oops, something went wrong!
```

Głębszy zanurzenie:
Pisanie na standardowe wyjście błędu jest praktykowane od dawna, gdyż jest to prosta i szybka metoda komunikacji z użytkownikiem. Alternatywnymi metodami są wykorzystanie specjalnego loggera lub zapisywanie błędów do pliku. Pisanie na standardowe wyjście błędu może być jednak bardziej wygodne dla programisty podczas szybkiej diagnostyki lub testowania kodu. W Ruby, STDERR (czyli standardowe wyjście błędu) jest globalną zmienną, więc nie wymaga ona dodatkowych importów lub konfiguracji.

Zobacz także:
- [Ruby Dokumentacja](https://ruby-doc.org/core-2.6.6/IO.html#class-IO-label-Standard+Streams)
- [Video o standardowych wyjściach w Ruby](https://www.youtube.com/watch?v=IqcbS0A9ORQ)