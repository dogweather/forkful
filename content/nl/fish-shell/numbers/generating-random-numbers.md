---
aliases:
- /nl/fish-shell/generating-random-numbers/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:56.730143-07:00
description: "Het genereren van willekeurige getallen is een fundamentele taak in\
  \ programmeren, gebruikt voor alles van gegevenssampling tot spelontwikkeling. In\
  \ Fish\u2026"
lastmod: 2024-02-18 23:09:02.318415
model: gpt-4-0125-preview
summary: "Het genereren van willekeurige getallen is een fundamentele taak in programmeren,\
  \ gebruikt voor alles van gegevenssampling tot spelontwikkeling. In Fish\u2026"
title: Willekeurige getallen genereren
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen is een fundamentele taak in programmeren, gebruikt voor alles van gegevenssampling tot spelontwikkeling. In Fish Shell maakt het gebruik van systeemtools en ingebouwde functies voor dit doel het voor programmeurs mogelijk om willekeur en variabiliteit effectief in scripts en applicaties te incorporeren.

## Hoe:

Het genereren van een willekeurig getal in Fish kan eenvoudig zijn, door het combineren van systeemhulpprogramma's en shell-mogelijkheden. Hieronder zijn enkele voorbeelden die demonstreren hoe willekeurige getallen binnen gespecificeerde bereiken te genereren.

**Genereer een willekeurig nummer tussen 0 en 100:**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**Voorbeelduitvoer:**
```fish
42
```

**Een willekeurig getal genereren tussen twee willekeurige getallen, bijvoorbeeld 50 en 150:**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**Voorbeelduitvoer:**
```fish
103
```

**Random gebruiken om een lijst te schudden:**

Je wilt misschien ook elementen in een lijst willekeurig schudden. Hier is hoe je dat kunt doen:

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**Voorbeelduitvoer:**
```fish
C
A
E
D
B
```

Houd er rekening mee dat de uitvoer elke keer dat je deze commando's uitvoert, zal variëren vanwege de aard van willekeur.

## Diepgaande Duik

De `random` functie van Fish Shell biedt een gebruiksvriendelijke interface voor het genereren van pseudo-willekeurige getallen. Intern maakt het gebruik van systeemniveau willekeurige getallengeneratie-hulpmiddelen en biedt het een draagbare manier om willekeur in je scripts te introduceren. Het is echter essentieel om te onthouden dat de willekeur die door `random` wordt geboden voldoende is voor de meeste scripttaken, maar misschien niet voldoet aan de cryptografische beveiligingseisen voor applicaties die een hogere mate van onvoorspelbaarheid nodig hebben.

Voor contexten met hoge beveiligingseisen, overweeg het gebruik van speciale tools of programmeerbibliotheken die ontworpen zijn voor cryptografische doeleinden, die sterkere garanties voor willekeur bieden. Desalniettemin, voor algemene scripts en applicaties waar de hoogste beveiligingsnormen voor willekeur geen vereiste zijn, biedt de `random` functie van Fish Shell een handige en effectieve oplossing.
