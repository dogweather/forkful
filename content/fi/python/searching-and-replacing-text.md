---
title:    "Python: Tekstin etsiminen ja korvaaminen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Miksi

Tekstin etsiminen ja korvaaminen on yleinen tehtävä ohjelmoinnissa, jota tarvitaan usein esimerkiksi tiedostojen tai tietokantojen käsittelyssä. Se voi myös säästää paljon aikaa ja vaivaa manuaalisen tekstikorjauksen sijaan.

# Kuinka

```Python
# Luo muuttuja teksti, jossa on haluttu teksti
teksti = "Tervetuloa blogiimme, rakkaat lukijat!"
# Korvaa "rakkaat" sanalla "arvostetut"
korjattu_teksti = teksti.replace("rakkaat", "arvostetut")
# Tulosta korjattu teksti
print(korjattu_teksti)
```

Tuloste: Tervetuloa blogiimme, arvostetut lukijat!

# Syväsukellus

Tekstin etsiminen ja korvaaminen tapahtuu usein käyttäen string-metodia "replace()". Tämä metodi ottaa kaksi argumenttia: etsittävän tekstin ja uuden tekstin, jolla halutaan korvata. Metodi palauttaa uuden stringin, jossa etsittävä teksti on korvattu halutulla tekstillä.

Esimerkiksi jos halutaan korvata kaikki esiintymät stringissä "Hello World" sanalla "Hei Maailma", niin se tapahtuu käyttämällä replace()-metodia seuraavasti:

```Python
# Luo muuttuja teksti, jossa on haluttu teksti
teksti = "Hello World, Hello World"
# Korvaa "Hello" sanalla "Hei"
korjattu_teksti = teksti.replace("Hello", "Hei")
# Tulosta korjattu teksti
print(korjattu_teksti)
```

Tuloste: Hei World, Hei World

Kuten huomataan, replace()-metodia voidaan käyttää myös useamman sanan korvaamiseen. Jos halutaan korvata vain tietyt esiintymät, voi käyttää myös toista string-metodia "split()", jolloin string jaetaan osiin halutun kohdan perusteella ja uusi string luodaan halutun tekstin ympärille. 

# Katso myös

- [Pythonin replace()-metodi](https://www.w3schools.com/python/ref_string_replace.asp)
- [String-metodit Pythonissa](https://www.w3schools.com/python/python_ref_string.asp)
- [Python-opas suomeksi](https://pythonworld.fi/)