---
title:                "Zahlen runden"
aliases:
- de/bash/rounding-numbers.md
date:                  2024-01-26T03:42:24.752922-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zahlen runden"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/rounding-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Zahlen zu runden bedeutet, die Dezimalstellen auf einen einfacheren Wert zu kürzen, der für einen gegebenen Kontext ausreichend gut ist. Programmierer runden Zahlen, um Ergebnisse zu vereinfachen, Speicherplatz zu sparen oder weil der genaue Wert nicht entscheidend ist – wie etwa, wenn man den CPU-Verbrauch oder den Speicherplatz überschlägt und Dezimalstellen Ihren Tag nicht beeinflussen.

## Wie zu:

Hier ist das Wichtigste zum Runden in Bash:

```Bash
# Abrunden mit 'floor' mittels bc
echo "scale=0; 3.49/1" | bc

# Aufrunden mit 'ceiling' mittels bc
echo "scale=0; 3.01/1" | bc -l

# Auf nächste ganze Zahl runden mit printf
printf "%.0f\n" 3.49

# Ein Trick, um auf die nächste ganze Zahl mit bc zu runden
echo "(3.49+0.5)/1" | bc
```

Beispielausgaben—direkt aus dem Terminal:

```
3  # Abgerundet (floor)
4  # Aufgerundet (ceiling)
3  # Auf nächste ganze Zahl gerundet (mit printf)
3  # Auf nächste ganze Zahl gerundet (mit bc)
```

## Tiefergehend

Früher gab es in Bash-Skripten kein `bc` oder `printf`, um die Mathe-Magie zu bewerkstelligen. Die Alteingesessenen mussten sich auf externe Tools oder kunstvolle Umwege verlassen. Jetzt ermöglicht `bc` präzise Mathematik. Dabei sollte man im Kopf behalten, dass `bc` standardmäßig nicht rundet – es führt eine Bodenfunktion durch. Der Teil mit der "scale" legt die Dezimalstellenaktion fest.

Alternativen? Man könnte `awk` für das Runden ohne Wechsel zu `bc` nutzen oder mit `perl` für anspruchsvollere Mathe-Bedürfnisse herumhantieren. Für Masochisten: Reines Bash mit, sagen wir, iterativer Zeichenkettenmanipulation – aber warum?

Was die Details betrifft, so rundet `bc` nicht nur, es macht jede Menge Mathe-Kram—es skaliert, sinust, wurzelt, was auch immer Sie wollen. Mit `printf` geht es mehr um das Formatieren von Text, aber hey, es rundet Zahlen, also beschweren wir uns nicht.

## Siehe auch

Für die, die mehr wollen:

- GNU `bc` Handbuch: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Bash `printf` Befehl: https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- AWK Benutzerhandbuch (für Rundungen und andere Textverarbeitungen): https://www.gnu.org/software/gawk/manual/gawk.html
- Mehr Mathematik, Skripte und Zahlentricks mit Bash: https://mywiki.wooledge.org/BashFAQ/022
