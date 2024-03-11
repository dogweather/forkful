---
date: 2024-01-26 04:45:25.507465-07:00
description: "Kompleksiluvut ovat lukujoukko muotoa `a + bi`, miss\xE4 `a` ja `b`\
  \ ovat reaalilukuja, ja `i` on imaginaariyksikk\xF6 (`i^2 = -1`). Ohjelmoinnissa\
  \ niit\xE4\u2026"
lastmod: '2024-03-11T00:14:30.063913-06:00'
model: gpt-4-0125-preview
summary: "Kompleksiluvut ovat lukujoukko muotoa `a + bi`, miss\xE4 `a` ja `b` ovat\
  \ reaalilukuja, ja `i` on imaginaariyksikk\xF6 (`i^2 = -1`). Ohjelmoinnissa niit\xE4\
  \u2026"
title: "Kompleksilukujen k\xE4sittely"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Kompleksiluvut ovat lukujoukko muotoa `a + bi`, missä `a` ja `b` ovat reaalilukuja, ja `i` on imaginaariyksikkö (`i^2 = -1`). Ohjelmoinnissa niitä käytetään ratkaisemaan ongelmia eri aloilla, kuten sähkötekniikassa, signaalinkäsittelyssä ja kvanttilaskennassa.

## Kuinka:
Python tukee sisäänrakennetusti kompleksilukuja. Tässä on tapa, jolla voit kokeilla niitä:

```Python
# Kompleksilukujen luominen
z = 4 + 5j
print(z)  # Tuloste: (4+5j)

# Reaaliosan ja imaginaariosan hakeminen
print(z.real)  # Tuloste: 4.0
print(z.imag)  # Tuloste: 5.0

# Kompleksilaskenta
w = 1 - 2j
print(z + w)  # Tuloste: (5+3j)
print(z - w)  # Tuloste: (3+7j)
print(z * w)  # Tuloste: (14+2j)
print(z / w)  # Tuloste: (-3.6+1.2j)

# Moduuli (itseisarvo)
print(abs(z))  # Tuloste: 6.4031242374328485

# Kompleksiluvun konjugaatti
print(z.conjugate())  # Tuloste: (4-5j)
```

## Syväsukellus
Kompleksiluvut konseptoitiin ensi kertaa 1500-luvulla Gerolamo Cardanon toimesta. Python, muiden ohjelmointikielien ohella, käsittelee kompleksilukuja ensisijaisina kansalaisina. Tämä tarkoittaa, että ne ovat sisäänrakennettuja kieleen, helppokäyttöisinä ominaisuuksina, välttäen tarpeen tuoda ulkopuolisia kirjastoja perustoimintoihin.

Kuitenkin, raskaaseen numeeriseen laskentaan, Pythonilla on kirjasto nimeltä `cmath`, joka on erityisesti kompleksiluvuille. Siinä on lisätoimintoja, kuten `exp`, `log` ja trigonometriset operaatiot.

Kun Python ei riitä, saatat kääntyä kirjastojen, kuten NumPyn, puoleen, erityisesti taulukko-operaatioihin liittyvissä asioissa, jotka sisältävät kompleksilukuja. NumPy tarjoaa optimoituja ja vektorisoituja operaatioita, jotka ovat elintärkeitä suorituskyvyn kannalta numeerisessa laskennassa.

## Katso myös
Tutustu näihin resursseihin oppiaksesi lisää:

- Pythonin virallinen dokumentaatio kompleksiluvuista: https://docs.python.org/3/library/stdtypes.html#typesnumeric
- `cmath`-moduulin dokumentaatio: https://docs.python.org/3/library/cmath.html
- NumPy kompleksilukujen sisältävien taulukoiden käsittelyyn: https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics
