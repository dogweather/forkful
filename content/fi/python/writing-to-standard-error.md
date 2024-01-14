---
title:    "Python: Kirjoittaminen standardivirheelle"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi
Miksi kirjoittaisit koodissasi standard erroriin (virheenkäsittelyvirtaan)? Standard Error on tärkeä osa koodaamista, joka auttaa sinua parantamaan ohjelmasi virheenkäsittelyä. Se näyttää sinulle, mitä tarkalleen tapahtuu, kun ohjelmasi kohtaa virheen, ja auttaa sinua jäljittämään ja korjaamaan ne nopeasti ja tehokkaasti.

## Miten
```Python
try:
    # Koodi, joka voi aiheuttaa virheen
except Exception as e:
    # Kirjoittaa virheen standard erroriin
    print("Virhe:", e, file=sys.stderr)
```

Koodin kirjoittaminen standard erroriin on yksinkertaista. Käytä vain ```print``` -funktiota ja anna sille toisena parametrina filen ```sys.stderr```. Tällä tavalla varmistat, että virhe ilmestyy standard erroriin eikä tulostaululle.

## Syvällinen sukellus
Jos haluat tarkempia tietoja ja hallintaa standard errorin käytöstä, voit käyttää Pythonin sisäistä ```traceback```-moduulia. Tämä moduuli auttaa sinua tulostamaan tarkemman virheviestin, joka sisältää virheen aiheuttaneen koodirivin ja tiedostonimen. Voit myös käyttää ```logging```-moduulia tallentaaksesi virheet tietokantaan tai lokitiedostoon.

## Katso myös
- Virheenkäsittely Pythonissa: https://realpython.com/python-handling-errors/
- Pythonin sys-moduuli: https://docs.python.org/3/library/sys.html
- "traceback" -moduulin käyttö: https://docs.python.org/3/library/traceback.html