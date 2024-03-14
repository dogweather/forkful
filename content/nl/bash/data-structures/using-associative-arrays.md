---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:04.655068-07:00
description: "Associatieve arrays zijn als super-opgeladen arrays die je toestaan\
  \ om strings te gebruiken als indexen in plaats van enkel integers. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:50.972548-06:00'
model: gpt-4-0125-preview
summary: "Associatieve arrays zijn als super-opgeladen arrays die je toestaan om strings\
  \ te gebruiken als indexen in plaats van enkel integers. Programmeurs\u2026"
title: Gebruik van associatieve arrays
---

{{< edit_this_page >}}

## Wat & Waarom?

Associatieve arrays zijn als super-opgeladen arrays die je toestaan om strings te gebruiken als indexen in plaats van enkel integers. Programmeurs gebruiken ze voor complexere datastructuren, waardoor het makkelijker wordt om met data om te gaan die niet netjes in een opeenvolgende lijst past.

## Hoe te gebruiken:

Allereerst, verklaar een associatieve array in Bash:

```Bash
declare -A my_array
```

Vervolgens kun je het beginnen te vullen met waarden, door strings als sleutels te gebruiken:

```Bash
my_array["name"]="Linux Journal"
my_array["topic"]="Programmeren"
```

Om een element te benaderen, gebruik je de sleutel:

```Bash
echo ${my_array["name"]}  # Geeft uit: Linux Journal
```

Itereren over sleutels en waarden is ook eenvoudig:

```Bash
for key in "${!my_array[@]}"; do
    echo "$key: ${my_array[$key]}"
done
```

Een voorbeelduitvoer zou er zo uit kunnen zien:

```
name: Linux Journal
topic: Programmeren
```

Om elementen toe te voegen of te wijzigen, wijs je gewoon een waarde toe aan een sleutel, op een soortgelijke manier als de initiële vulling:

```Bash
my_array["readers"]="Jij"
```

En om een element te verwijderen, gebruik `unset`:

```Bash
unset my_array["topic"]
```

## Diep Duiken

Associatieve arrays werden geïntroduceerd in Bash versie 4.0, waardoor ze een relatief recente toevoeging aan de taal zijn. Voor hun introductie was het omgaan met niet-integer index arrays lastig, vaak vereisend omweggetjes of externe tools zoals `awk` of `sed`.

Onder de motorkap implementeert Bash associatieve arrays met behulp van hash tabellen. Deze implementatie maakt een efficiënte sleutelopzoeking mogelijk, die vrij constant blijft ongeacht de grootte van de array, een cruciaal kenmerk voor prestaties in scriptuitvoering.

Hoewel associatieve arrays in Bash veel kracht en flexibiliteit bieden aan shell-scripting, komen ze met hun eigen set beperkingen, zoals enigszins onhandiger zijn om mee te werken in vergelijking met arrays in hogere programmeertalen zoals Python of JavaScript. Voor complexe datamanipulatietaken kan het nog steeds het overwegen waard zijn om externe tools of talen te gebruiken die beter geschikt zijn voor de klus.

Echter, voor veel typische scripttaken bieden associatieve arrays een waardevol hulpmiddel in de toolkit van de Bash-programmeur, waardoor scripts leesbaarder en onderhoudbaarder worden door het toestaan van het gebruik van betekenisvolle string sleutels in plaats van numerieke indexen.
