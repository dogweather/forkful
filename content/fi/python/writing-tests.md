---
title:                "Testien kirjoittaminen"
html_title:           "Python: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-tests.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Ohjelmistokehittäjät käyttävät testien kirjoittamista varmistaakseen, että koodi toimii odotetulla tavalla ja välttääkseen mahdolliset virheet.

## Miten:
Alla on esimerkki yksinkertaisesta testistä Python-koodissa, joka testaa funktiota, joka laskee kahden luvun summan ja palauttaa arvon:

```Python
def sum(a, b):
  return a + b
  
def test_sum():
  assert sum(2, 3) == 5
  assert sum(-1, 1) == 0
```

Testien suorittamiseksi voidaan käyttää esimerkiksi pytest-kirjastoa, joka antaa selkeän raportin testien tuloksista:

```Python
===================== test session starts =====================
platform linux -- Python 3.8.1, pytest-6.2.4, py-1.10.0, pluggy-0.13.1
collected 1 item

test_example.py .                                         [100%]

====================== 1 passed in 0.01s ======================
```

## Syvempää tietoa:
Testien kirjoittaminen on tärkeä osa ohjelmistokehitystä, sillä se auttaa varmistamaan koodin toimivuuden ja vähentämään virheitä. Aiemmin testaaminen tapahtui manuaalisesti, mutta nykyään testauskirjastot ja automaattiset testit ovat yleistyneet ja helpottaneet kehittäjien työtä. Alternatiivisia tapoja testata koodia ovat esimerkiksi yksikkötestaus ja testausrobotit.

Kun halutaan varmistaa kattava testikattavuus, on tärkeää suunnitella testit huolella ja testata myös rajoitustapaukset ja virheelliset syötteet. Testien kirjoittaminen on myös hyvä tapa dokumentoida koodia ja helpottaa muiden kehittäjien koodin ymmärtämistä ja muokkaamista.

## Katso myös:
- https://docs.pytest.org - pytest-dokumentaatio
- https://www.samueltaylor.org/articles/understanding-pytest-fixtures/ - tietoa pytest-kirjaston käytöstä
- https://www.youtube.com/watch?v=byS7tuHxMgQ - video testien tärkeydestä ja kirjoittamisesta