---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:53.168649-07:00
description: "Code opsplitsen in functies betekent scripts onderverdelen in kleinere,\
  \ herbruikbare blokken die specifieke taken uitvoeren. Het maakt de code netter,\u2026"
lastmod: '2024-03-13T22:44:50.988490-06:00'
model: gpt-4-0125-preview
summary: Code opsplitsen in functies betekent scripts onderverdelen in kleinere, herbruikbare
  blokken die specifieke taken uitvoeren.
title: Code organiseren in functies
weight: 18
---

## Wat & Waarom?
Code opsplitsen in functies betekent scripts onderverdelen in kleinere, herbruikbare blokken die specifieke taken uitvoeren. Het maakt de code netter, begrijpelijker en gemakkelijker om te debuggen.

## Hoe te:
Maak een eenvoudige functie in Bash:

```Bash
greet() {
  echo "Hallo, $1!"
}
```

Gebruik het door de functie aan te roepen met een parameter:

```Bash
greet "Wereld"  # Uitvoer: Hallo, Wereld!
```

Functies kunnen retourwaarden geven met `return` voor numerieke statuscodes (niet voor daadwerkelijke datateruggave):

```Bash
add() {
  return $(($1 + $2))
}

add 3 4
echo $?  # Uitvoer: 7
```

Let op dat `$?` de retourwaarde van de laatste opdracht vastlegt, dat is het numerieke resultaat van `add`.

## Diepgaand
In Bash zijn functies al sinds de vroege versies een manier om code te compartimenteren. Historisch gezien, is het gebruik van functies in lijn met de principes van gestructureerd programmeren die in de jaren 60 werden geïntroduceerd om de codekwaliteit te verbeteren.

Alternatieven voor functies zijn onder andere het sourcen van scriptbestanden of het gebruik van aliassen, maar deze bieden niet hetzelfde niveau van modulariteit en hergebruik.

Een opvallend implementatiedetail in Bash is dat functies als eersteklas burgers worden beschouwd; ze hebben geen specifiek declaratiewoord zoals `function` in andere talen, hoewel `function` optioneel is in Bash voor de leesbaarheid. Ook de scope van de functie is interessant—variabelen zijn standaard globaal, tenzij ze als lokaal zijn gedeclareerd, wat tot onverwacht gedrag kan leiden als het niet goed wordt beheerd.

## Zie Ook
- Bash-handleiding over Shellfuncties: https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html
- Geavanceerde Bash-scriptinggids: https://tldp.org/LDP/abs/html/functions.html
- "Pro Bash Programming: Scripting the GNU/Linux Shell" voor diepgaande concepten en praktijken van functiescripting.
