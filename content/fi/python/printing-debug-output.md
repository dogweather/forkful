---
title:                "Tulostetaan vianmääritystulosteita"
html_title:           "Python: Tulostetaan vianmääritystulosteita"
simple_title:         "Tulostetaan vianmääritystulosteita"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Tiedämme kaikki, että koodin debuggaaminen voi olla haastavaa ja aikaa vievää. Siksi on tärkeää käyttää kaikkia käytettävissä olevia työkaluja ja tekniikoita helpottamaan tätä prosessia. Yksi hyödyllinen tapa on tulostaa debug-lähtöä.

## Miten!

```python
def laske_pinta_ala(pituus, leveys):
  pinta_ala = pituus * leveys
  print("Pinta-ala on: ", pinta_ala)

laske_pinta_ala(5, 10)
```

Kun suoritamme tätä koodinpätkää, saamme seuraavan tulosteen:

`Pinta-ala on: 50`

Tulostamalla debug-lähtöä pystymme seuraamaan koodin suoritusta ja löytämään mahdollisia virheitä tai epäloogisuuksia. Näin voimme helposti paikantaa ja korjata ongelmia koodissamme.

## Syvemmälle

Tulostettavan debug-lähdön määrä ja tarkkuus voivat vaihdella riippuen tilanteesta ja tarpeista. Yleensä paras tapa on tulostaa tiettyjä muuttujia tai arvoja, kun haluamme tarkistaa niiden arvoja koodissamme. Tämä voi auttaa meitä selvittämään, onko arvo oikea ja missä kohtaa koodia se on asetettu.

On myös hyödyllistä lisätä tietoa tulostettuihin rivinumeroihin tai muihin tunnisteisiin, jotta voimme helposti löytää kyseisen kohdan koodista. Tämä voi säästää paljon aikaa, kun etsimme tiettyä koodiriviä suuresta tiedostosta.

## Katso myös

[Pythonin virallinen dokumentaatio debug-lähtöön](https://docs.python.org/3.9/library/builtins.html#print)

[Tutorialspoint: Python Debugging](https://www.tutorialspoint.com/python/python_debugging.htm)

[RealPython: How to Debug Python Like a Pro](https://realpython.com/python-debugging-pdb/)