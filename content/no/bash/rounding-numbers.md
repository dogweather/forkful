---
title:                "Avrunding av tall"
date:                  2024-01-26T03:42:59.130157-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrunding av tall"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/rounding-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å runde av tall betyr å kutte av desimalene til en enklere verdi som er god nok for en gitt kontekst. Programmerere runder av tall for å forenkle resultater, spare plass, eller fordi den eksakte verdien ikke er avgjørende – som når du skjønnsmessig vurderer CPU-bruk eller diskplass, og desimaler ikke vil gjøre dagen din bedre eller verre.

## Hvordan:

Her er det grunnleggende om avrunding i Bash:

```Bash
# Rund ned med 'floor' med bc
echo "scale=0; 3.49/1" | bc

# Rund opp med 'ceiling' med bc
echo "scale=0; 3.01/1" | bc -l

# Rund til nærmeste hele tall med printf
printf "%.0f\n" 3.49

# Et triks for å runde til nærmeste hele tall med bc
echo "(3.49+0.5)/1" | bc
```

Eksempelutskrifter—rett fra terminalens munn:

```
3  # Rundet ned (floor)
4  # Rundet opp (ceiling)
3  # Rundet til nærmeste (med printf)
3  # Rundet til nærmeste (med bc)
```

## Dyp Dykk

Tilbake i dagen, fantes det ikke `bc` eller `printf` i Bash-skript for å utføre matte-magi. Gamle helter måtte stole på eksterne verktøy eller smarte omveier. Nå lar `bc` deg utføre presisjonsmatematikk. Husk, `bc` runder ikke av som standard - den utfører floor-operasjonen. Scale-delen setter desimalpunktshandlingen.

Alternativer? Du kunne bruke `awk` for avrunding uten å bytte til `bc`, eller bryne deg på `perl` for tyngre mattebehov. For de masochistiske, gå rent Bash med, la oss si, iterativ strengbehandling – men hvorfor?

Når det gjelder detaljer, runder ikke `bc` bare av, den utfører hauger av matte-greier—skalert den, sin den, sqrt den, du nevner det. Med `printf` er det mer om tekstformatering, men hei, det runder av tall, så vi klager ikke.

## Se Også

For de som er sultne på mer:

- GNU `bc` manual: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Bash `printf` kommando: https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- AWK brukerhåndbok (for avrunding og annen tekstprosessering): https://www.gnu.org/software/gawk/manual/gawk.html
- Mer Bash matte, skripting, og talltriks: https://mywiki.wooledge.org/BashFAQ/022
