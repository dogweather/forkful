---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:16.930717-07:00
description: "Debug output afdrukken gaat allemaal om het naar de console echo\xEB\
  n van data om te checken wat er gaande is in je script. Programmeurs doen dit om\u2026"
lastmod: '2024-03-11T00:14:24.819636-06:00'
model: gpt-4-0125-preview
summary: "Debug output afdrukken gaat allemaal om het naar de console echo\xEBn van\
  \ data om te checken wat er gaande is in je script. Programmeurs doen dit om\u2026"
title: Debug-output afdrukken
---

{{< edit_this_page >}}

## Wat & Waarom?

Debug output afdrukken gaat allemaal om het naar de console echoën van data om te checken wat er gaande is in je script. Programmeurs doen dit om variabelen te volgen, de logica stroom te volgen en die vervelende bugs te spotten.

## Hoe te:

```Bash
#!/bin/bash

# Definieer een variabele
name="Gizmo"

# Print variabele voor debugging
echo "Debug: De variabelenaam is $name"

# Voorwaardelijk met debug output
if [[ $name == "Gizmo" ]]; then
    echo "Debug: Het if-statement is betreden."
    # Doe iets
fi

# Lus met debug output
for i in {1..3}; do
    echo "Debug: Lusiteratie $i"
    # Doe iets in lus
done
```

Output:
```
Debug: De variabelenaam is Gizmo
Debug: Het if-statement is betreden.
Debug: Lusiteratie 1
Debug: Lusiteratie 2
Debug: Lusiteratie 3
```

## Diepgaande duik

Oorspronkelijk betekende debuggen het verwijderen van fysieke bugs die vroege computers verstoorden. Tegenwoordig gaat het om het verpletteren van code bugs. Debug outputs zijn het vergrootglas van de programmeur.

Alternatieven voor `echo` in bash-scripts zijn onder andere `printf` voor meer formatteringsopties of schrijven naar een bestand met omleiding `>` voor blijvende logs.

Bash ondersteunt ook conditionele debug output met de ingebouwde `set -x` om commando's en hun argumenten te traceren zoals ze worden uitgevoerd. `set -x` is geweldig voor volledige script debugging.

## Zie ook

- De `man` pagina van Bash: `man bash`
- Geavanceerde scriptgids: [Bash Guide for Beginners door Machtelt Garrels](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- Stack Overflow voor probleemoplossing: [stackoverflow.com](https://stackoverflow.com/questions/tagged/bash)
