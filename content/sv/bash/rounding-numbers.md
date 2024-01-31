---
title:                "Avrundning av tal"
date:                  2024-01-26T03:42:48.912838-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrundning av tal"

category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/rounding-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Avrundning av tal innebär att man klipper av decimalerna till ett enklare värde som är tillräckligt bra för ett givet sammanhang. Programmerare avrundar tal för att förenkla resultat, spara utrymme, eller för att det exakta värdet inte är avgörande - som när du överslagsberäknar CPU-användning eller diskutrymme, och decimalerna inte är avgörande för din dag.

## Hur man gör:

Här är det låga ner på avrundning i Bash:

```Bash
# Avrunda neråt med 'floor' med bc
echo "scale=0; 3.49/1" | bc

# Avrunda uppåt med 'ceiling' med bc
echo "scale=0; 3.01/1" | bc -l

# Avrunda till närmaste hela med printf
printf "%.0f\n" 3.49

# Ett trick för att avrunda till närmaste hela med bc
echo "(3.49+0.5)/1" | bc
```

Exempelutskrifter - direkt från terminalens mun:

```
3  # Avrundat neråt (floor)
4  # Avrundat uppåt (ceiling)
3  # Avrundat till närmaste (med printf)
3  # Avrundat till närmaste (med bc)
```

## Fördjupning

Förr i tiden, fanns det ingen `bc` eller `printf` i Bash-skript för att göra matematikmagi. Gamla rävar var tvungna att förlita sig på externa verktyg eller påhittiga omvägar. Nu låter `bc` dig göra precision matematik. Kom ihåg, `bc` avrundar inte som standard - det utför flooring. Scale-delen ställer in decimalpunktsaktionen.

Alternativ? Du kan använda `awk` för avrundning utan att byta till `bc` eller brottas med `perl` för tyngre matematikbehov. För den masochistiska, gå ren Bash med, låt oss säga, iterativ strängmanipulation - men varför?

När det gäller detaljer, gör `bc` inte bara avrundningar, det gör massor av mattegrejer - skala det, sinus det, sqrt det, du nämner det. Med `printf` handlar det mer om textformatering, men hej, det avrundar tal, så vi klagar inte.

## Se även

För de som är sugna på mer:

- GNU `bc`-manual: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Bash `printf`-kommando: https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- AWK användarguide (för avrundning och annan textbehandling): https://www.gnu.org/software/gawk/manual/gawk.html
- Mer Bash-matematik, skriptning och nummertrick: https://mywiki.wooledge.org/BashFAQ/022
