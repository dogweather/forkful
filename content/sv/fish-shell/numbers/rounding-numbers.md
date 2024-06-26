---
date: 2024-01-26 03:44:12.702706-07:00
description: "Hur man g\xF6r: I Fish h\xE4nger avrundning av tal p\xE5 `math`-kommandot.\
  \ Anv\xE4nd `math -s0` f\xF6r att avrunda till n\xE4rmaste heltal."
lastmod: '2024-03-13T22:44:38.330687-06:00'
model: gpt-4-0125-preview
summary: "I Fish h\xE4nger avrundning av tal p\xE5 `math`-kommandot."
title: Avrundning av tal
weight: 13
---

## Hur man gör:
I Fish hänger avrundning av tal på `math`-kommandot. Använd `math -s0` för att avrunda till närmaste heltal.

```fish
# Avrunda uppåt
echo (math -s0 "4.7")
# Utdata: 5

# Avrunda nedåt
echo (math -s0 "4.3")
# Utdata: 4

# Avrunda till två decimalplatser
echo (math -s2 "4.5678")
# Utdata: 4.57

# Avrunda negativt tal
echo (math -s0 "-2.5")
# Utdata: -3
```

## Djupdykning
Historiskt sett gjordes avrundning av tal mer manuellt eller med externa verktyg, men i moderna skal som Fish är det inbyggt i inbyggda verktyg. Fishs tillvägagångssätt med användning av `math`-kommandot förenklar saker jämfört med äldre skal. Alternativ i andra programmeringsmiljöer varierar; språk som Python använder funktioner som `round()`, medan Bash kan kräva mer komplexa uttryck eller `bc`-verktyget. Fishs implementering av avrundning förenklar skriptning genom att hålla matematiken inom skal-miljön istället för att anropa andra verktyg eller språk.

## Se även
- Fish-dokumentation för `math`-kommandot: https://fishshell.com/docs/current/cmds/math.html
- IEEE-standard för flyttalsaritmetik (IEEE 754): https://ieeexplore.ieee.org/document/4610935
