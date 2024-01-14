---
title:    "Python: Tulostaminen debug-ulosanto"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Ihmiset usein ymmärtävät ohjelmoinnin olevan vain koodien kirjoittamista ja virheiden korjaamista. Mutta mitä jos kerran koodi ei toimikaan niin kuin pitäisi? Tämä on kun tulostus virhe viestejä (debug output) tulee pelastajaksi. Tulostus virhe viesti on prosessi, jossa ohjelmaan lisätään lisäkoodia, jotta voidaan saada tietoa mitä ohjelmassa tapahtuu ja missä vaiheessa se menee pieleen.

## Miten

Tulostus virhe viestejä voidaan lisätä ohjelmaan yksinkertaisilla "print" komennoilla. Esimerkiksi, jos haluat nähdä tietyn muuttujan arvon tiettynä hetkenä, voit lisätä koodiisi seuraavanlaisen rivin:

```Python
print(muuttuja)
```

Tämä tulostaa muuttujan arvon konsoliin, jolloin voit tarkistaa sen oikeellisuuden. Voit myös lisätä tekstiä tulosteen sekaan selittämään paremmin mitä tapahtuu:

```Python
print("Muuttujan arvo on:", muuttuja)
```

Tämän avulla voit seurata ohjelman suorittamista ja havaita mikä aiheuttaa ongelman.

## Syvällinen sukellus

Tulostus virhe viestien lisääminen ohjelmaan voi auttaa sinua löytämään ja korjaamaan virheitä nopeammin. Voit valita tarkalleen missä kohdissa tulostus tapahtuu ja mitä tietoja haluat nähdä. Näin voit tutkia ohjelman suoritusta vähän syvemmältä ja ymmärtää paremmin mitä tapahtuu.

Tärkeää on myös muistaa poistaa tulostus virhe viestit ohjelmasta, kun olet löytänyt ja korjannut virheet. Muuten koodisi saattaa olla turhan tilava ja hidastaa ohjelman suoritusta.

## Katso myös

- https://www.codecademy.com/articles/how-to-debug-python
- https://realpython.com/python-print/
- https://www.python.org/dev/peps/pep-0020/