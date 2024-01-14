---
title:    "Fish Shell: Skrive til standardfeil"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville man ønske å skrive til standardfeilen? Vel, noen ganger ønsker man å skrive ut feil og advarsler separat fra vanlig utdata. Dette kan hjelpe med feilsøking og gjøre det enklere å finne og håndtere problemer.

## Hvordan gjøre det

Fish Shell tilbyr en enkel og effektiv måte å skrive til standardfeilen på. Her er et eksempel på hvordan du kan skrive tekst til standardfeilen:

```Fish Shell
echo "Dette er en feilmelding" >&2
```

Merk at vi bruker `>&2` for å sende utdata til standardfeilen i stedet for standardutdataen, som er den vanlige utgangen. Dette vil føre til at teksten vises som en feilmelding i terminalen.

Vi kan også bruke denne funksjonen innenfor andre kommandoer. For eksempel, hvis vi ønsker å skrive til standardfeilen i en løkke, kan vi gjøre dette:

```Fish Shell
for i in (seq 1 10)
  echo "Feil ved iterasjon $i" >&2
end
```

Dette vil skrive ut en feilmelding for hver iterasjon i løkken.

## Dykk dypere

En ting å være oppmerksom på er at hvis standardfeilen ikke er koblet til en konsoll, vil feilmeldinger og advarsler ikke vises. Dette betyr at hvis du kjører et script i bakgrunnen eller i en cron job, vil du ikke kunne se disse meldingene. Du kan imidlertid lagre dem i en fil ved å bruke `>>` i stedet for `>&2`.

En annen nyttig funksjon er å skrive feilmeldinger til en loggfil. Dette er spesielt nyttig hvis du har et stort skript som kjører over lang tid. Ved å skrive feilmeldinger til en loggfil, kan du enkelt kontrollere eventuelle problemer som oppstår i etterkant.

## Se også

- [Fish Shell sin offisielle nettside](https://fishshell.com/)
- [Kom i gang med Fish Shell](https://fishshell.com/docs/current/tutorial.html) (på engelsk)
- [Feilsøking i Fish Shell](https://fishshell.com/docs/current/faq.html#faq-troubleshooting) (på engelsk)