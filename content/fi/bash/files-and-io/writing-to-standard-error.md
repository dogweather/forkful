---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:20.433142-07:00
description: "Kirjoittaminen vakiovirheeseen (stderr) Bashissa tarkoittaa virheviestien\
  \ tai muiden t\xE4rkeiden diagnostisten tulosteiden ohjaamista erilleen\u2026"
lastmod: '2024-02-25T18:49:53.666748-07:00'
model: gpt-4-0125-preview
summary: "Kirjoittaminen vakiovirheeseen (stderr) Bashissa tarkoittaa virheviestien\
  \ tai muiden t\xE4rkeiden diagnostisten tulosteiden ohjaamista erilleen\u2026"
title: Kirjoittaminen standardivirheeseen
---

{{< edit_this_page >}}

## Mikä ja miksi?
Kirjoittaminen vakiovirheeseen (stderr) Bashissa tarkoittaa virheviestien tai muiden tärkeiden diagnostisten tulosteiden ohjaamista erilleen vakiotulosteesta (stdout). Ohjelmoijat tekevät näin varmistaakseen, että virheviestit voidaan helposti tunnistaa, lokittaa tai jopa ohittaa, mikä auttaa vianetsintä- ja lokitusprosesseissa.

## Kuinka:
Bashissa käytät `>&2` uudelleenohjataksesi tulosteen stderr:iin. Tässä on perusesimerkki:

```bash
echo "Tämä on normaali viesti"
echo "Tämä on virheviesti" >&2
```

Tämän skriptin suorittaminen näyttää molemmat viestit konsolissa, mutta jos ohjaat ne uudelleen, voit erottaa stdout:n stderr:stä. Esimerkiksi:

```bash
bash script.sh > output.txt 2> error.txt
```

`output.txt` sisältää `"Tämä on normaali viesti"`, kun taas `error.txt` sisältää `"Tämä on virheviesti"`.

Käytännön esimerkissä katsotaan skriptiä, joka käsittelee tiedostoja ja raportoi virheen, jos tiedostoa ei ole olemassa:

```bash
filename="example.txt"

if [ ! -f "$filename" ]; then
    echo "$filename ei ole olemassa!" >&2
    exit 1
else
    echo "Käsitellään $filename"
fi
```

Esimerkkituloste suoraan konsolissa, kun `example.txt` ei ole olemassa:

```
example.txt ei ole olemassa!
```

Bashissa ei ole suoria kolmannen osapuolen kirjastoja stderr:n käsittelyyn, sillä uudelleenohjaus on natiivisti tuettu ja yleensä riittävä. Kuitenkin monimutkaisissa sovelluksissa voidaan käyttää lokituskehyksiä tai ulkoisia lokitusvälineitä, kuten `syslog` tai `log4bash`, jotta sekä stdout että stderr voidaan hallita tehokkaammin.
