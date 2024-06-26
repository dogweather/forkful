---
date: 2024-01-20 17:32:12.845879-07:00
description: "How to: (Kuinka tehd\xE4:) Esimerkki tuloste."
lastmod: '2024-04-05T21:53:58.326370-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Esimerkki tuloste."
title: "Kahden p\xE4iv\xE4m\xE4\xE4r\xE4n vertailu"
weight: 27
---

## How to: (Kuinka tehdä:)
```Bash
#!/bin/bash
# Muoto YYYY-MM-DD

pvm1="2023-03-15"
pvm2="2023-04-01"

if [[ "$pvm1" > "$pvm2" ]]; then
    echo "pvm1 on myöhemmin kuin pvm2."
elif [[ "$pvm1" < "$pvm2" ]]; then
    echo "pvm1 on aikaisemmin kuin pvm2."
else
    echo "pvm1 ja pvm2 ovat samat."
fi
```
Esimerkki tuloste:
```
pvm1 on aikaisemmin kuin pvm2.
```

## Deep Dive (Syväsukellus):
Bashissa, joka on syntynyt 1989, päivämäärien vertailuun on monta tapaa. Käytimme yksinkertaista leksikaalista vertailua koska päivämäärät ovat standardissa ISO 8601 -muodossa. Jos muoto vaihtelee, `date`-komennolla voidaan muuttaa eri formaatteja sekuntteihin siitä hetkestä, kun Unix-aika alkoi (1.1.1970), ja vertailla näitä.

Vaihtoehtoisesti:
```Bash
# Muunnetaan päivämäärät sekunneiksi
sek_pvm1=$(date -d "$pvm1" +%s)
sek_pvm2=$(date -d "$pvm2" +%s)

# Vertailu
if [ "$sek_pvm1" -gt "$sek_pvm2" ]; then
    echo "pvm1 on myöhemmin kuin pvm2."
elif [ "$sek_pvm1" -lt "$sek_pvm2" ]; then
    echo "pvm1 on aikaisemmin kuin pvm2."
else
    echo "pvm1 ja pvm2 ovat samat."
fi
```

Ymmärtäminen, että päivämäärien vertailu tekstijonoina onnistuu, koska Bash vertaa niitä leksikografisesti. Tämä tarkoittaa, että merkkijonoja verrataan niiden järjestyksessä, aakkos-numeerisesti.

## See Also (Katso myös):
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/)
- [Bash manuaali](https://www.gnu.org/software/bash/manual/bash.html)
- [Unix-aika, `date`-komennon käyttö](https://www.unixtimestamp.com/)
