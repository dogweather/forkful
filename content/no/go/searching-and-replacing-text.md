---
title:                "Go: Søk og erstatt tekst"
simple_title:         "Søk og erstatt tekst"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du arbeider med programvareutvikling eller tekstbehandling, er det ofte nødvendig å finne og erstatte bestemte deler av en tekst. Dette kan være tidkrevende og feilfylt hvis du gjør det manuelt. Ved å bruke søke- og erstattingsfunksjonaliteten i Go-programmeringsspråket, kan du effektivt gjøre endringer i store mengder tekst.

## Slik gjør du det

For å søke og erstatte tekst i Go, kan du bruke den innebygde replace-funksjonen. Denne funksjonen tar inn tre parametere: originalteksten, hva som skal søkes etter, og hva som skal erstattes med. For eksempel:

```Go
text := "Hei, hvordan har du det?"
nyTekst := strings.Replace(text, "har du det", "går det", -1)
fmt.Println(nyTekst)
```
Output: Hei, hvordan går det?

I dette eksempelet erstattet vi "har du det" med "går det" i teksten. Det fjerde parameteret er antall ganger teksten skal erstattes, hvis du ikke er sikker på antall forekomster, kan du bruke "-1".

## Dypdykk

Når du bruker replace-funksjonen, er det viktig å merke seg at den er case-sensitive. Dette betyr at hvis teksten inneholder store og små bokstaver, må du følge dette nøyaktig. Hvis du ønsker å gjøre søk og erstatt på en tekst uavhengig av store og små bokstaver, kan du bruke toLower-funksjonen i kombinasjon med replace. For eksempel:

```Go
text := "I dag skal vi på fottur."
nyTekst := strings.Replace(strings.ToLower(text), "på fottur", "til parken", -1)
fmt.Println(nyTekst)
```
Output: I dag skal vi til parken.

Ved å bruke toLower-funksjonen, vil teksten bli konvertert til små bokstaver, og deretter kan du søke etter det du ønsker å erstatte uavhengig av store og små bokstaver.

## Se også

- Go offisiell nettside: https://golang.org/
- Strings-pakken i Go: https://golang.org/pkg/strings/
- Gode tips for å bruke replace-funksjonen i Go: https://gobyexample.com/replace