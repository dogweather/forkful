---
title:    "Fish Shell: Jämföra två datum"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en viktig del av programmering, speciellt när man arbetar med datumbaserade data. Det kan hjälpa till att hitta skillnader i datum och utföra olika åtgärder baserat på resultaten.

## Hur man gör

För att jämföra två datum i Fish Shell, kan du använda funktionen `date` tillsammans med operatorn `-s` som står för "subtraktion". Till exempel, om vi vill jämföra dagens datum med datumet för två veckor sedan, skulle koden se ut så här:

```Fish Shell
date -s "-2 weeks"
```

Detta kommer att ta dagens datum och dra bort två veckor från det. Om du vill jämföra två specifika datum, måste du först konvertera dem till Unix-tidsstämplar och sedan subtrahera dem med `-s` operatorn. Använd sedan `echo` för att se resultatet. Till exempel:

```Fish Shell
echo (date -f '%s' '2021-01-01') - (date -f '%s' '2020-12-01')
```

Detta kommer att ge dig differensen i sekunder mellan de två datumen.

## Djupdykning

Det finns flera sätt att jämföra två datum i Fish Shell:

- Du kan använda `-lt`, `-le`, `-eq`, `-ge`, `gt` operatorerna tillsammans med `date` för att utföra mindre än, mindre än eller lika med, större än, mer än eller lika med jämförelser.

- Du kan använda `strftime` för att konvertera datum till andra format för enkel jämförelse.

- Du kan också använda en sekundär variabel för att hålla datumet som ska jämföras, istället för att använda `date` varje gång.

Var noga med att kontrollera dokumentationen för mer information och andra användbara funktioner för att jämföra datum i Fish Shell.

## Se även

- [Fish Shell dokumentation] (https://fishshell.com/docs/current/index.html)

- [Unix-tidsstämpel] (https://en.wikipedia.org/wiki/Unix_time)

- [Jämföra datum i Bash Shell] (https://www.howtogeek.com/662683/how-to-compare-dates-in-bash/)