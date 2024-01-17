---
title:                "Leser en tekstfil"
html_title:           "Ruby: Leser en tekstfil"
simple_title:         "Leser en tekstfil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Lesetilgang av tekstfiler er en viktig ferdighet for programmerere. Det innebærer å lese innholdet i en tekstfil og behandle det på en strukturert måte. Dette er nyttig for å kunne lagre, manipulere og analysere store mengder data.

## Hvordan:

For å lese en tekstfil i Ruby, kan du bruke den innebygde metoden `File.read`. Denne metoden tar imot filbanen og returnerer innholdet i filen som en streng.

```Ruby
innhold = File.read("filsti/til/tekstfil.txt")
puts innhold
```

Output:
```
Dette er en tekstfil.
Her kan du lese noen linjer med tekst.
```

## Dypdykk:

Å lese tekstfiler har vært en viktig del av programmering helt siden starten. Det har vært nødvendig for å kunne lagre og behandle data på en effektiv måte. Selv om det finnes alternative måter å lese filer på, som for eksempel ved å bruke tredjeparts biblioteker, er den innebygde metoden `File.read` fortsatt den mest vanlige metoden for å lese tekstfiler i dag.

Når det gjelder implementasjon, bruker metoden `File.read` en filbunt for å få tilgang til innholdet i filen. Dette gjør det mulig å lese filen innholdet bit-for-bit og behandle det på en mer effektiv måte enn å lese hele filen inn i hukommelsen på en gang.

## Se også:

* [Ruby File Class Documentation](https://ruby-doc.org/core-2.7.1/File.html)
* [Ruby File Methods](https://ruby-doc.org/core-2.7.1/File.html#method-i-read)
* [Ruby IO Class Documentation](https://ruby-doc.org/core-2.7.1/IO.html)