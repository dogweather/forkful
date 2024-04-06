---
date: 2024-01-20 17:30:51.285777-07:00
description: "Miten: Bash-komentorivill\xE4 p\xE4iv\xE4m\xE4\xE4rien laskeminen k\xE4\
  ytt\xE4\xE4 `date` komentoa, joka on ollut k\xE4yt\xF6ss\xE4 jo Unix-j\xE4rjestelmien\
  \ alkuaikoina. Vaihtoehtoina\u2026"
lastmod: '2024-04-05T22:51:10.904891-06:00'
model: gpt-4-1106-preview
summary: "Bash-komentorivill\xE4 p\xE4iv\xE4m\xE4\xE4rien laskeminen k\xE4ytt\xE4\xE4\
  \ `date` komentoa, joka on ollut k\xE4yt\xF6ss\xE4 jo Unix-j\xE4rjestelmien alkuaikoina."
title: "Tulevan tai menneen p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
weight: 26
---

## Miten:
```Bash
# Tulevaisuuden päivämäärän laskeminen (3 päivää nykyhetkestä)
päivämäärä_3_päivää="$(date -d "+3 days" +%Y-%m-%d)"
echo "Kolmen päivän päästä: $päivämäärä_3_päivää"

# Menneisyyden päivämäärän laskeminen (5 päivää sitten)
viisi_päivää_sitten="$(date -d "5 days ago" +%Y-%m-%d)"
echo "Viisi päivää sitten: $viisi_päivää_sitten"
```
Esimerkkitulostus:
```
Kolmen päivän päästä: 2023-04-14
Viisi päivää sitten: 2023-04-06
```

## Syväsukellus
Bash-komentorivillä päivämäärien laskeminen käyttää `date` komentoa, joka on ollut käytössä jo Unix-järjestelmien alkuaikoina. Vaihtoehtoina `date`:lle löytyy ohjelmia kuten `DateTime` Perlissä tai `datetime` Pythonissa. Implementaatiotiedoissa on huomioitava, että päivämäärien laskennassa tulee ottaa huomioon karkausvuodet ja aikavyöhykkeet. Bashin `date` käsittelee nämä automaattisesti.

## Katso Myös
- GNU Coreutils `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Advanced Bash-Scripting Guide: Date Commands: https://www.tldp.org/LDP/abs/html/dates.html
- Stack Overflow - Questions about Bash date calculations: https://stackoverflow.com/questions/tagged/date+calculation+bash
