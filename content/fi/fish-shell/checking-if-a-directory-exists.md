---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Fish Shell: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Tarkistaminen, onko hakemisto olemassa, on yksi ohjelmoijien yleisimmistä tehtävistä. Tämä auttaa varmistamaan, että tarvittavat tiedostot ja hakemistot ovat saavutettavissa ja toimivat oikein. Tarkistamalla, onko hakemisto olemassa, voit myös estää virheitä ohjelmassa.

## Kuinka tehdä:

Fish Shell -tapa esimerkkikoodiin - "Rikitse se mitä hakemiston palauttama tuloste on ...

```
if test -d <hakemiston_nimi>
    echo "Hakemisto on olemassa"
else
    echo "Hakemistoa ei ole olemassa"
end
```

Esimerkki tulosteesta, jos hakemisto on olemassa: 
```
Hakemisto on olemassa
```

Esimerkki tulosteesta, jos hakemistoa ei ole olemassa: 
```
Hakemistoa ei ole olemassa
```

## Syvällisempi sukellus:

Historiallinen konteksti:
Tarkistamisen tarve johtuu siitä, että joskus ohjelmoijat haluavat varmistaa, että tietty hakemisto on olemassa ennen kuin he suorittavat jotain toimintoa. Tämä auttaa estämään virheitä ohjelmassa.

Vaihtoehtoja:
Fish Shell tarjoaa muita tapoja tarkistaa, onko hakemisto olemassa. Yksi vaihtoehto on käyttää komentoa `test -e <hakemiston_nimi>`, joka tarkistaa, onko tiedosto tai hakemisto olemassa.

Toteutuksesta:
Fish Shell käyttää standardia UNIX-komentojen kuten `test` ja `if` komentoriviä. Se tarkistaa, onko tiedosto tai hakemisto olemassa ja tulostaa vastaavan viestin sen perusteella.

## Katso myös:

[Määritä ja hanki tämä suurimmista ohjelmointitermeistä, selitettynä](https://zapier.com/blog/programming-terms-glossary/)