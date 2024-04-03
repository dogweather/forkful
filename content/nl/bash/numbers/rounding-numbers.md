---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:41.332875-07:00
description: "Het afronden van getallen betekent het afhakken van de decimalen tot\
  \ een eenvoudigere waarde die goed genoeg is voor een bepaalde context. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:50.974507-06:00'
model: gpt-4-0125-preview
summary: Het afronden van getallen betekent het afhakken van de decimalen tot een
  eenvoudigere waarde die goed genoeg is voor een bepaalde context.
title: Afronden van getallen
weight: 13
---

## Hoe te:
Hier is de informatie over afronden in Bash:

```Bash
# Naar beneden afronden met 'floor' met bc
echo "scale=0; 3.49/1" | bc

# Naar boven afronden met 'ceiling' met bc
echo "scale=0; 3.01/1" | bc -l

# Afronden naar de dichtstbijzijnde hele met printf
printf "%.0f\n" 3.49

# Een trucje om naar de dichtstbijzijnde hele af te ronden met bc
echo "(3.49+0.5)/1" | bc
```

Voorbeelduitvoeren—rechtstreeks uit de mond van de terminal:

```
3  # Naar beneden afgerond (floor)
4  # Naar boven afgerond (ceiling)
3  # Afgerond naar de dichtstbijzijnde (met printf)
3  # Afgerond naar de dichtstbijzijnde (met bc)
```

## Diepduiken
Vroeger was er geen `bc` of `printf` in Bash-scripts om wiskundige trucs te doen. Oudgedienden moesten vertrouwen op externe tools of slimme workarounds. Nu, `bc` laat je precisierekenkunde doen. Bedenk wel, `bc` rondt niet standaard af—it maakt gebruik van afronding naar beneden. Het scale deel bepaalt de actie van het decimaalteken.

Alternatieven? Je zou `awk` kunnen gebruiken voor afronding zonder over te schakelen naar `bc` of worstelen met `perl` voor zwaardere wiskundige behoeften. Voor de masochistische onder ons, ga puur Bash met, laten we zeggen, iteratieve tekenreeksmanipulatie - maar waarom?

Wat betreft details, `bc` doet niet alleen afronding, het doet veel wiskundige dingen - schaal het, sinus het, wortel het, noem maar op. Met `printf`, gaat het meer over tekst formatteren, maar hé, het rondt getallen af, dus we klagen niet.

## Zie ook
Voor degenen die meer willen weten:

- GNU `bc` handleiding: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Bash `printf` commando: https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- AWK gebruikersgids (voor afronding en andere tekstverwerking): https://www.gnu.org/software/gawk/manual/gawk.html
- Meer Bash wiskunde, scripting en getaltrucs: https://mywiki.wooledge.org/BashFAQ/022
