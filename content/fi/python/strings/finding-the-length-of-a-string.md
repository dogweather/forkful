---
date: 2024-01-20 17:48:17.685023-07:00
description: "Stringin pituuden selvitt\xE4minen tarkoittaa merkkijonon merkkien lukum\xE4\
  \xE4r\xE4n laskemista. Koodarit tekev\xE4t t\xE4t\xE4 validoinnin, muotoilun ja\
  \ silmukoiden\u2026"
lastmod: 2024-02-19 22:05:15.048531
model: gpt-4-1106-preview
summary: "Stringin pituuden selvitt\xE4minen tarkoittaa merkkijonon merkkien lukum\xE4\
  \xE4r\xE4n laskemista. Koodarit tekev\xE4t t\xE4t\xE4 validoinnin, muotoilun ja\
  \ silmukoiden\u2026"
title: "Merkkijonon pituuden selvitt\xE4minen"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Stringin pituuden selvittäminen tarkoittaa merkkijonon merkkien lukumäärän laskemista. Koodarit tekevät tätä validoinnin, muotoilun ja silmukoiden hallinnan helpottamiseksi.

## Kuinka tehdään:
Tässä pari esimerkkiä stringin pituuden määrittämiseen Python-koodilla:

```python
# Esimerkki 1: Perus käyttö
merkkijono = "Hei Suomi"
pituus = len(merkkijono)
print(pituus)  # Output: 9

# Esimerkki 2: Silmukassa stringien läpikäynti
sanat = ["moi", "terve", "hei"]
for sana in sanat:
    print(f"{sana} on {len(sana)} merkkiä pitkä.")
```

Tulostus:

```
9
moi on 3 merkkiä pitkä.
terve on 5 merkkiä pitkä.
hei on 3 merkkiä pitkä.
```

## Syväsukellus
Pythonin `len`-funktio on nopea ja tehokas tapa saada selville objektin, kuten stringin, pituus. Historiallisesti, tämän funktionaliteetin taustalla on C-kielen `strlen()`-funktio, mutta Pythonin `len` on toteutettu suoraan Pythonin tulkissa, jolloin se on paljon nopeampi.

Vaihtoehtoja `len`-funktiolle on vähän, mutta jos haluat itse toteuttaa toiminnallisuuden, voit käyttää silmukkaa:

```python
def oma_len_funktio(merkkijono):
    pituus = 0
    for _ in merkkijono:
        pituus += 1
    return pituus

# Käytetään itse tehtyä funktiota
print(oma_len_funktio("Heippa!"))  # Output: 7
```

Stringin pituuden löytäminen käyttäen `len` on sisäänrakennettu, turvallinen ja ennen kaikkea hyvin integroitu muihin Pythonin osiin – se toimii listojen, joukkojen, sanakirjojen ja muiden iterointikelpoisten objektien kanssa.

## Katso myös
Pythonin virallinen dokumentaatio `len`-funktiosta: https://docs.python.org/3/library/functions.html#len

Stack Overflow keskusteluja ja esimerkkejä stringien käsittelystä: https://stackoverflow.com/questions/tagged/python+string

W3Schools Python String Length -opas: https://www.w3schools.com/python/ref_func_len.asp
