---
title:                "Tekstin etsiminen ja vaihtaminen"
html_title:           "Python: Tekstin etsiminen ja vaihtaminen"
simple_title:         "Tekstin etsiminen ja vaihtaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Tekstien etsiminen ja korvaaminen on tärkeä osa ohjelmoinnissa käytettyä toimintoa. Se mahdollistaa nopean ja automatisoidun tavan löytää ja muuttaa tiettyä tekstiä koodissa. Tämä säästää aikaa ja vaivaa, jotka muutoin kuluisivat manuaalisen etsimisen ja korvaamisen parissa.

## Kuinka tehdä?
Tekstien etsiminen ja korvaaminen onnistuu Pythonin avulla helposti. Alla olevassa esimerkissä käytämme replace()-funktiota, joka etsii ja korvaa kaikki "word" -tekstit "string" -tekstillä.
```Python
teksti = "Tämä on esimerkkilause"
uusi_teksti = teksti.replace("esimerkkilause", "uusi lause")
print(uusi_teksti)
```
Tulostuksena näkyy nyt: "Tämä on uusi lause".

## Syvempi sukellus
Tekstien etsiminen ja korvaaminen on ollut tärkeä osa ohjelmointimaailmaa jo pitkään. Ennen Pythonin replace()-funktiota, eri ohjelmointikielissä oli erilaisia tapoja suorittaa tämä toiminto. Joissain kielissä se oli hyvin monimutkaista ja vaati paljon koodia, mutta Pythonin ansiosta se on nyt yksinkertaista ja helppoa. Korvaamisen lisäksi Pythonin re.sub()-funktio tarjoaa myös ominaisuuden hakea ja korvata tietyn kaavan mukaan tekstistä. Lisäksi, Pythonin avulla pystyy suorittamaan etsimis- ja korvaamistoimintoja myös tiedostoissa.

## Katso myös
Tässä on muutamia lisälähteitä tietojen etsimisen ja korvaamisen aiheista Pythonissa:
- ["Pythonin tekstien käsittely" -opas](https://dev.to/petercour/learn-python-text-processing-1jdk) (englanniksi)
- [Regex101-sivusto](https://regex101.com/) - apuväline tekstien etsimisen ja korvaamisen kaavojen luomiseen (englanniksi)
- [Pythonin virallinen ohjesivusto](https://docs.python.org/3/howto/regex.html) - tiedot Pythonin re-moduulista ja sen tarjoamista toiminnoista tekstin käsittelyyn (englanniksi)