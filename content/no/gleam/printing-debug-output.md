---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Gleam: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor vil du bruke tid på å legge til utskriftsfeil i koden din? Vel, feilsøking er en nødvendig del av enhver programmerers jobb, og debug-informasjon er en enkel og effektiv måte å finne og rette feil på. Uten utskriftsfeil kan du bruke unødvendig mye tid på å lete etter og rette feil, noe som kan føre til frustrasjon og dårligere kode.

## Hvordan gjøre det

For å legge til utskriftsfeil i Gleam, bruker du funksjonen `debug!` med en verdi eller variabel du vil skrive ut som argument. For eksempel:

```Gleam
debug!(navn)
```

Dette resulterer i at verdien av variabelen `navn` blir skrevet ut i debug-informasjonen. Du kan også skrive ut en kombinasjon av tekst og variabler ved å bruke en interpolert streng, som dette:

```Gleam
debug!("Bestillingsnummer: {}", bestillingsnummer)
```

Dette vil skrive ut en melding som inkluderer både teksten "Bestillingsnummer:" og verdien av variabelen `bestillingsnummer`.

## Dypdykk

Hvis du vil ha mer kontroll over hvordan debug-informasjonen vises, kan du bruke funksjonen `debug` i stedet for `debug!`. Dette lar deg velge et format for utskriften, for eksempel JSON, og angi en fil eller strøm som utgang. Du kan også velge å deaktivere utskrift i produksjonsmiljøet ved å bruke `debug_disabled`.

## Se også

- [Offisiell dokumentasjon for debug-modulen](https://gleam.run/modules/gleam_debug/latest)
- [Video tutorial om å legge til debug-informasjon i Gleam](https://www.youtube.com/watch?v=ocqBMDl7xZY) 
- [Gleam slack community - #debug channel](https://gleam-lang.slack.com/archives/C01HS9MGLG0)