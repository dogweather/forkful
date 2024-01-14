---
title:                "Python: Tulostetaan virheenkorjaustulos"
simple_title:         "Tulostetaan virheenkorjaustulos"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Debug-tulostus on tärkeä osa ohjelmointiprosessia, ja se auttaa löytämään virheitä ja ongelmia koodissasi. Se on myös hyvä tapa seurata koodin suorituskykyä ja muokata sitä tarpeen mukaan.

## Miten tehdä

Debug-tulostus on helppoa Pythonissa. Voit käyttää `print()`-funktiota tulostamaan haluamasi tiedot. Esimerkiksi:

```python
name = "Hannu"
age = 25

print("Nimeni on", name)
print("Olen", age, "vuotta vanha")
```

Tämä tuottaa seuraavanlaisen tulostuksen:

```
Nimeni on Hannu
Olen 25 vuotta vanha
```

Voit myös käyttää `f`-merkkiä merkkijonon alussa ja sijoittaa haluamasi muuttujan sulkeiden sisään kirjoittaaksesi sen arvon suoraan tulostukseen:

```python
print(f"Olen {age} vuotta vanha ja minun nimeni on {name}")
```

Tämä tuottaa saman tulostuksen kuin edellinen esimerkki.

Voit myös tulostaa muuttujien tyypit käyttämällä `type()`-funktiota:

```python
print(type(name))
print(type(age))
```

Tämä tuottaa seuraavanlaisen tulostuksen:

```
<class 'str'>
<class 'int'>
```

## Syväsukellus

Debug-tulostusta voi käyttää monilla tavoilla, kuten algoritmien optimointiin ja virheiden etsimiseen. Voit myös tulostaa muiden muuttujien lisäksi esimerkiksi silmukkien suorituskertojen lukumäärää tai ehtolauseiden totuusarvoja.

Debug-tulostusta voidaan myös käyttää hyödyllisten tietojen tulostamiseen ohjelman suorituksen aikana. Esimerkiksi voit tulostaa tietyissä kohdissa ohjelmassa merkkijonon, joka kertoo missä kohtaa ohjelma on menossa ja mitä sillä hetkellä tapahtuu.

Vaikka debug-tulostuksen käyttö voi joskus tuntua kiusalliselta ja turhalta, se voi todella auttaa sinua saamaan syvemmän käsityksen koodistasi ja ongelmien ratkaisemiseen.

## Katso myös
- [Pythonin virallinen dokumentaatio tulostamisesta](https://docs.python.org/fi/3/tutorial/inputoutput.html#more-on-printing)
- [Debug-tulostuksen hyödyntäminen ohjelmoinnissa](https://realpython.com/python-debugging-pdb/)
- [Hyviä käytäntöjä debug-tulostuksen käyttöön](https://stackify.com/python-debugging-tips/)