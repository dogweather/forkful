---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:35.869506-07:00
description: "Kuinka: Python sis\xE4lt\xE4\xE4 sis\xE4\xE4nrakennetun moduulin testeille\
  \ nimelt\xE4 `unittest`. N\xE4in voit k\xE4ytt\xE4\xE4 sit\xE4 yksinkertaisen funktion\
  \ testaamiseen."
lastmod: '2024-03-13T22:44:56.147222-06:00'
model: gpt-4-0125-preview
summary: "Python sis\xE4lt\xE4\xE4 sis\xE4\xE4nrakennetun moduulin testeille nimelt\xE4\
  \ `unittest`."
title: Testien kirjoittaminen
weight: 36
---

## Kuinka:
Python sisältää sisäänrakennetun moduulin testeille nimeltä `unittest`. Näin voit käyttää sitä yksinkertaisen funktion testaamiseen:

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "Pitäisi olla 12")

if __name__ == '__main__':
    unittest.main()
```

Kun ajat tämän testiskriptin, sinun tulisi nähdä tuloste, jossa ilmoitetaan, että testisi menivät läpi (tai epäonnistuivat).

Nykyajan moderneja ja ilmaisuvoimaisempia testejä varten voit käyttää kolmannen osapuolen kirjastoa, kuten `pytest`. Ensin sinun on asennettava se käyttäen pip:

```shell
pip install pytest
```

Sen jälkeen voit kirjoittaa testisi yksinkertaisemmin ilman, että sinun tarvitsee periä mitään:

```python
# Tallenna tämä tiedostoon nimeltä test_with_pytest.py
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "Pitäisi olla 12"
```

Ajaa testisi `pytest`in kanssa, suorita yksinkertaisesti:

```shell
pytest test_with_pytest.py
```

Sinun pitäisi nähdä pytestin tuloste testituloksistasi.
