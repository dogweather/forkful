---
date: 2024-01-26 04:40:18.552964-07:00
description: "Komplexa tal utvidgar id\xE9n om en endimensionell nummerlinje till\
  \ ett tv\xE5dimensionellt komplext plan. Programmerare anv\xE4nder dem inom omr\xE5\
  den som\u2026"
lastmod: '2024-02-25T18:49:36.644955-07:00'
model: gpt-4-0125-preview
summary: "Komplexa tal utvidgar id\xE9n om en endimensionell nummerlinje till ett\
  \ tv\xE5dimensionellt komplext plan. Programmerare anv\xE4nder dem inom omr\xE5\
  den som\u2026"
title: Att arbeta med komplexa tal
---

{{< edit_this_page >}}

## Vad & Varför?
Komplexa tal utvidgar idén om en endimensionell nummerlinje till ett tvådimensionellt komplext plan. Programmerare använder dem inom områden som ingenjörsvetenskap, fysik och grafik för beräkningar som kräver två komponenter, som signaler eller rotationer.

## Hur man gör:
I Fish hanterar vi komplexa tal med `math` med verkliga och imaginära delar. Här är en start:

```fish
# Lägg till två komplexa tal (3+4i) och (5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # Ger ut: 8+6i

# Multiplicera två komplexa tal (1+2i) och (3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # Ger ut: -5+10i
```

Om du behöver upphöja ett komplext tal till en potens eller få dess exponentiella form:

```fish
# Kvadraten av (2+3i)
set complex_square (math "(2+3i)^2")
echo $complex_square # Ger ut: -5+12i

# Exponentialen av (2i)
set complex_exp (math "e^(2i)")
echo $complex_exp # Ger ut: -0.41615+0.9093i
```

## Djupdykning
Fish Shells stöd för komplexa tal är relativt nytt, med början runt version 3.1.0. Innan dess kanske folk har använt `bc` eller anropat externa verktyg som Python för komplex matematik.

Alternativ till Fishs matematik inkluderar specialiserade numeriska bibliotek eller språk som MATLAB, Python med NumPy, eller till och med C++ med Standardbiblioteket. Dock kan dessa vara överdrivna för snabba skalberäkningar.

Fishs stöd för komplexa tal är inbyggt i dess interna `math`-kommando, som utnyttjar libcalc. Det betyder att du inte behöver installera extra verktyg för grundläggande operationer.

Dock är Fish inte utformat för tung matematisk beräkning. Dess matematikförmåga är bekväm för snabba beräkningar eller skript där komplexa tal kommer till spel, men överväg mer robusta verktyg för intensiva uppgifter.

## Se även
- Fish shell-dokumentation för math: https://fishshell.com/docs/current/commands.html#math
- NumPy för Python, ett populärt alternativ: https://numpy.org/
- En djupare titt på komplexa tal: https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
