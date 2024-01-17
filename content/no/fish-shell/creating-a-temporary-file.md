---
title:                "Oppretting av en midlertidig fil"
html_title:           "Fish Shell: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Skape en midlertidig fil er en måte å lagre midlertidig informasjon på mens du utfører en kode. Dette er nyttig når du trenger å lagre data midlertidig for å bruke senere i koden din. Programmører bruker dette for å strukturere og organisere koden sin på en effektiv måte.

## Hvordan:

Det er enkelt å lage en midlertidig fil i Fish Shell. Du kan bruke kommandoen "mktemp" og angi et filnavn eller et prefiks for å generere en unik midlertidig fil.

```
Fish Shell: mktemp -t mytempfile
```

Dette vil opprette en midlertidig fil med navnet "mytempfile", som vil være plassert i mappen "/tmp". Du kan deretter bruke denne filen til å lagre midlertidig informasjon i koden din. Når koden din er ferdig å kjøre, vil den midlertidige filen automatisk bli slettet.

## Dypdykk:

Lagring av midlertidig informasjon er en vanlig praksis i programmering, og det finnes forskjellige metoder for å oppnå dette. En annen måte å lage en midlertidig fil på er å bruke kommandoen "touch". Dette vil opprette en tom fil som kan brukes til midlertidig lagring.

Fish Shell støtter også flere kommandoer for å arbeide med midlertidige filer, som for eksempel å flytte, kopiere eller slette dem. Dette gjør det enkelt å arbeide med midlertidige filer i koden din.

## Se også:

- [Offisiell Dokumentasjon for Fish Shell Temporary Files](https://fishshell.com/docs/current/index.html#temporary-files)
- [Diskusjon om Midlertidige Filer på GitHub](https://github.com/fish-shell/fish-shell/issues/4622)
- [Blogginnlegg om Bruk av Midlertidige Filer i Fish Shell](https://blog.fishshell.com/2019/02/15/temporary-files/)