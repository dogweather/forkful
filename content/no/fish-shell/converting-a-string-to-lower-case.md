---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Fish Shell: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor 
Hvorfor vil du konvertere en streng til små bokstaver? Det er enkelt - for å endre tekst til en mer lesbar og konsistent form.

## Hvordan
Bruk følgende kode i Fish Shell for å konvertere en streng til små bokstaver:

```fish
set string "GRØNN FISK"
echo $string | tr '[:upper:]' '[:lower:]'
```

Dette vil gi følgende output:

```fish
grønn fisk
```

## Dypdykk
Metoden som brukes i eksempelet ovenfor, bruker kommandoen "tr" for å konvertere alle store bokstaver ([:upper:]) til små bokstaver ([:lower:]). Dette er en enkel, men effektiv måte å konvertere en streng til små bokstaver på. 

Det finnes også andre måter å oppnå dette på i Fish Shell, for eksempel ved bruk av kommandoen "sed". Du kan lese mer om dette og andre metoder i Fish Shell sin dokumentasjon.

## Se også
- [Offisiell dokumentasjon for Fish Shell](https://fishshell.com/docs/current/index.html)
- [Grunnleggende bruk av terminalen](https://www.dreamhost.com/blog/the-command-line-is-your-friend/)
- [Utforsk forskjellige kommandoer i Fish Shell](https://fishshell.com/docs/current/cmds.html)