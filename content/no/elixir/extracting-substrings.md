---
title:    "Elixir: Uttrekking av delstrenger"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor?

Du lurer kanskje på hvorfor du bør engasjere deg i å ekstrahere substringer i Elixir programmering. Vel, substringer er en viktig del av mange programmer og er svært nyttige når man ønsker å manipulere tekst. Ved å lære hvordan man ekstraherer substringer, kan du gjøre din kode mer effektiv og funksjonell.

## Slik gjør du det:

For å ekstrahere substringer i Elixir, kan du bruke funksjonen `slice/3`. Denne funksjonen tar inn en liste, en startindeks og en slutindeks, og returnerer en del av listen som tilsvarer substrider fra og med startindeksen til, men ikke inkludert, slutindeksen.

```Elixir
my_list = [1, 2, 3, 4, 5]
slice(my_list, 1, 3) # Output: [2, 3]
```

Du kan også bruke `slice/2` for å ekstrahere en del av en liste fra en bestemt indeks til slutten av listen.

```Elixir
slice(my_list, 3) # Output: [4, 5]
```

Hvis du trenger å ekstrahere en del av en streng i Elixir, kan du bruke `String.slice/3` eller `String.slice/2` på samme måte som med lister.

```Elixir
my_string = "Elixir programmering er gøy!"
String.slice(my_string, 7, 19) # Output: "programmering"
String.slice(my_string, 13) # Output: "gøy!"
```

## Dypdykk:

Det er viktig å merke seg at Elixir bruker null-indeks som standard for lister og strenger, noe som betyr at det første elementet har indeks 0. Dette betyr at hvis du vil ekstrahere første element i en liste eller streng, må du bruke indeks 0. Det er også viktig å huske på at indekser er inkludert i ekstraksjonen, men ikke sluttpunktet.

Videre kan du også bruke negative indekser for å ekstrahere substringer fra slutten av listen eller strengen. For eksempel, hvis du bruker `-1` som slutindeks, vil substriden bli ekstrahert fra starten av listen eller strengen til og med det nest siste elementet. 

```Elixir
String.slice(my_string, 0, -3) # Output: "Elixir programmering er"
```

## Se også:

- [Elixir Dokumentasjon for `slice/3`](https://hexdocs.pm/elixir/List.html#slice/3)
- [Elixir Dokumentasjon for `slice/2`](https://hexdocs.pm/elixir/List.html#slice/2)
- [Elixir Dokumentasjon for `String.slice/3`](https://hexdocs.pm/elixir/String.html#slice/3)
- [Elixir Dokumentasjon for `String.slice/2`](https://hexdocs.pm/elixir/String.html#slice/2)