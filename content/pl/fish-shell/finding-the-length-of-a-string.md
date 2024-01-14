---
title:    "Fish Shell: Znajdowanie długości ciągu znaków"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś powinien zajmować się szukaniem długości ciągu znaków? Proste - jest to podstawowa umiejętność w programowaniu, której często używamy w różnych sytuacjach. Może to być przydatne, na przykład, w weryfikowaniu poprawności wprowadzonych danych lub w manipulowaniu tekstem w specyficzny sposób.

## Jak to zrobić

Zanim przejdziemy do głębszych wód, spójrzmy najpierw na prosty kod, który pomoże nam znaleźć długość ciągu znaków. W przypadku Fish Shell, możemy tego dokonać za pomocą funkcji `strlen`:

```Fish Shell
set string "To jest przykładowy ciąg znaków"
echo (strlen $string)
```

Po uruchomieniu powyższego kodu, otrzymamy wynik 32, co jest dokładną długością naszego ciągu znaków. Proste prawda?

## Deep Dive

Jeśli jesteś ciekaw, jak dokładnie działa funkcja `strlen` w Fish Shell, poniżej znajdziesz krótkie wyjaśnienie. Tak naprawdę, ta funkcja jest dostępna w języku C i została zaprojektowana tak, aby zliczać każdy znak w ciągu, aż nie napotka znaku zakończenia, czyli `NULL`. W Fish Shell, `NULL` jest oznaczony za pomocą pojedynczego znaku spacji, dlatego też wywołanie `strlen` na zmiennej zawierającej tekst zakończony spacją, zwróci długość bez spacji.

## Zobacz także

- Dokumentacja funkcji `strlen`: https://fishshell.com/docs/current/cmds/strlen.html
- Poradnik na temat manipulacji tekstem w Fish Shell: https://fishshell.com/docs/current/tutorial.html#manipulating-text