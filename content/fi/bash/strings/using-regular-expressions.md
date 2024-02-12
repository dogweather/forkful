---
title:                "Säännöllisten lausekkeiden käyttö"
aliases: - /fi/bash/using-regular-expressions.md
date:                  2024-02-03T19:16:13.089329-07:00
model:                 gpt-4-0125-preview
simple_title:         "Säännöllisten lausekkeiden käyttö"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Säännölliset lausekkeet (regex) Bashissa mahdollistavat merkkijonojen ja tiedostojen etsimisen, manipuloinnin ja käsittelyn tiettyjen mallien perusteella. Ohjelmoijat käyttävät regexiä tehtäviin kuten syötteen validointi, lokitiedostojen jäsentäminen ja datan poiminta, koska se tarjoaa joustavan ja voimakkaan tavan määritellä malleja monimutkaisiin tekstinkäsittelytarpeisiin.

## Kuinka:

### Perusmallin Vastaavuus
Löytääksesi, vastaako merkkijonoa malli, voit käyttää `grep`-komentoa, komentorivin työkalua, joka etsii tavallisen tekstin datajoukoista rivejä, jotka vastaavat säännöllistä lauseketta:

```bash
echo "Hei, Maailma!" | grep -o "Maailma"
# Tuloste: Maailma
```

### Tietyn Datan Poimiminen
Poimiaksesi datan osia, jotka vastaavat regex-mallejasi, voit käyttää `-o`-vaihtoehtoa yhdessä `grep`-komentojen kanssa:

```bash
echo "Virhe: Tiedostoa ei löydy" | grep -oE "[A-Za-z]+:"
# Tuloste: Virhe:
```

### Säännöllisten Lausekkeiden Käyttö `sed`-komentojen kanssa
`sed` (stream editor) on tehokas työkalu tekstin jäsentämiseen ja muokkaamiseen. Näin voit käyttää `sed`-komentoa regexin kanssa korvataksesi tekstiä:

```bash
echo "Bash on mahtava" | sed -e 's/mahtava/upea/'
# Tuloste: Bash on upea
```

### Mallien Vastaavuus Ehdollisissa Lauseissa
Bash tukee myös regexiä suoraan ehdollisissa lausunnoissa:

```bash
[[ "https://esimerkki.com" =~ ^https?:// ]] && echo "URL on validi" || echo "URL on invalidi"
# Tuloste: URL on validi
```

### Edistynyt Mallien Vastaavuus ja Manipulaatio `awk`:n avulla
`awk` on toinen tekstinkäsittelytyökalu, joka tukee monimutkaisempaa datan poimintaa ja manipulointia. Se voi olla hyödyllinen työskenneltäessä rakenteisen tekstidatan, kuten CSV-tiedostojen kanssa:

```bash
echo -e "ID,Nimi,Ikä\n1,John,22\n2,Jane,24" | awk -F, '$3 > 22 {print $2 " on vanhempi kuin 22."}'
# Tuloste: Jane on vanhempi kuin 22.
```

Vaikka Bashin sisäänrakennetut regex-toiminnot kattavat monia käyttötapauksia, erittäin edistyneitä regex-operaatioita varten saattaa olla hyödyllistä käyttää yhdistelmää Bash-skripteistä `perl`- tai `python`-skriptien kanssa, koska nämä kielet tarjoavat voimakkaita regex-kirjastoja (esim. `re` Pythonissa). Yksinkertainen esimerkki Pythonilla:

```bash
echo "Taltioi tämä 123" | python3 -c "import sys; import re; print(re.search('(\d+)', sys.stdin.read()).group(0))"
# Tuloste: 123
```

Näiden ohjelmointikielten tarvittaessa sisällyttäminen voi auttaa sinua hyödyntämään regexin täyttä voimaa Bash-skripteissäsi.
