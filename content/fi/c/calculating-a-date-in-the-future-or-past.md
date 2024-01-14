---
title:    "C: Tulevan tai menneen päivämäärän laskeminen"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin projekteissa saattaa olla tarve laskea tietty päivämäärä tulevaisuudessa tai menneisyydessä. Tämä voi olla hyödyllistä esimerkiksi aikataulujen suunnittelussa tai tapahtumien ennustamisessa. C-kieli tarjoaa useita tapoja laskea päivämääriä, joten se on hyvä kieli oppia perusajatuksista.

## Miten

Päivämäärien laskeminen C-kielellä vaatii muutaman perusasian ymmärtämistä. Ensinnäkin, päivämäärät tallennetaan yleensä taulukkoina, jotka sisältävät päivä, kuukausi ja vuosi. Tätä voidaan esittää myös numeroina, esimerkiksi 29.6.2021 on sama kuin 29, 6, 2021. 

Kun halutaan laskea päivämäärä tulevaisuudessa, voidaan käyttää C:n sisäänrakennettuja funktioita, kuten `mktime` ja `localtime`. Nämä funktiot tarvitsevat syötekenttiä, kuten vuoden, kuukauden ja päivän, ja palauttavat arvoja, jotka edustavat päivämäärän ajanjaksoa. Esimerkiksi `mktime` voi ottaa yksinkertaisen päivämäärän, kuten 1.1.2022 ja laskea sille vastaavan ajanjakson, joka voidaan sitten tuoda esille esimerkiksi `printf`-funktion avulla.

## Syvällinen sukellus

Päivämäärien laskeminen C-kielellä voi olla monimutkaisempaa kuin vain yksittäisten funktioiden käyttöä. Jos haluat tarkempia päivämääriä, voit käyttää C:n aikandrakennuspaketteja, kuten `time.h`. Näillä pakkauksilla voit määrittää aikavälejä ja käyttää monimutkaisempia laskutoimituksia päivämäärien kanssa. Tämä voi auttaa sinua luomaan tarkempia ja täsmällisempiä päivämääriä, jotka sopivat tarkoitukseesi täydellisesti.

## Katso myös

- [C-kielen perusteet] (https://www.tut.fi/~jl6592/Tie-23500/kieli/c.html)
- [Time and Date -toimintojen opetusohjelma C-kielessä] (https://www.programiz.com/c-programming/c-date-time)
- [C:n ajanjaksojen ja päivämäärien käsittelyn opas] (https://www.gnu.org/software/libc/manual/html_node/Time-Types.html)