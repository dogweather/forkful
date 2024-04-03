---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:01.124295-07:00
description: "Associatieve arrays, of hash maps, laten je data opslaan als sleutel-waarde\
  \ paren, wat het makkelijker maakt om informatie te organiseren en op te halen\u2026"
lastmod: '2024-03-13T22:44:51.238045-06:00'
model: gpt-4-0125-preview
summary: Associatieve arrays, of hash maps, laten je data opslaan als sleutel-waarde
  paren, wat het makkelijker maakt om informatie te organiseren en op te halen op
  basis van sleutel.
title: Gebruik van associatieve arrays
weight: 15
---

## Wat & Waarom?

Associatieve arrays, of hash maps, laten je data opslaan als sleutel-waarde paren, wat het makkelijker maakt om informatie te organiseren en op te halen op basis van sleutel. Ze zijn handig voor wanneer je een gestructureerdere manier nodig hebt om met data om te gaan dan alleen lijsten, vooral in configuraties en bij het omgaan met een scala aan attributen.

## Hoe:

Fish ondersteunt van nature geen associatieve arrays zoals Bash 4+, maar je kunt een vergelijkbare functionaliteit bereiken met een combinatie van lijsten en stringmanipulatie. Hier is hoe je ze kunt nabootsen:

Eerst, het opzetten van "associatieve array" elementen apart:

```Fish Shell
set food_color_apple "rood"
set food_color_banana "geel"
```

Om een element te benaderen, verwijs je er direct naar:

```Fish Shell
echo $food_color_apple
# Uitvoer: rood
```

Als je erover moet itereren, gebruik dan een for-loop met inachtneming van een naamgevingsconventie:

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# Uitvoer:
# rood
# geel
```

Voor degenen die Bash's `${!array[@]}` missen om alle sleutels te krijgen, kun je sleutels in een aparte lijst opslaan:

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'is' $food_color_$key
end
# Uitvoer:
# apple is rood
# banana is geel
```

## Diepgaande Duik

Echt associatieve arrays zoals in andere scripttalen zijn nog geen onderdeel van Fish's benadering. De getoonde work-around maakt gebruik van Fish's stringmanipulatie en lijstcapaciteiten om een pseudo-associatieve arraystructuur te creëren. Hoewel het werkt, is het niet zo schoon of foutbestendig als ingebouwde ondersteuning voor associatieve arrays zou zijn. Andere shells zoals Bash en Zsh bieden ingebouwde functionaliteit voor associatieve arrays, wat resulteert in meer rechttoe rechtaan, leesbare code. Echter, Fish's ontwerpfilosofie streeft naar eenvoud en gebruiksvriendelijkheid, mogelijk ten koste van dergelijke functies. De work-around voldoet aan de meeste behoeften, maar houd de evolutie van Fish Shell in de gaten—zijn ontwikkelaars verbeteren actief en voegen functies toe op basis van feedback van de gemeenschap.
