---
title:    "Python: Sattumanvaraisten lukujen luominen"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi

Satunnaisluvut ovat tehokas työkalu monissa eri ohjelmointitehtävissä. Ne voivat auttaa meitä luomaan monipuolisia ja toistettavia tuloksia, kokeilemaan erilaisia skenaarioita ja parantamaan ohjelmien suorituskykyä. Python tarjoaa useita tapoja luoda satunnaislukuja, ja tässä blogikirjoituksessa opimme, miten voit hyödyntää niitä omassa koodissasi.

## Kuinka

```Python
import random 

# Generoi ja tulosta yksi satunnainen kokonaisluku väliltä 0-100
random_number = random.randint(0, 100)
print(random_number)

# Generoi ja tulosta lista, jossa on 5 satunnaista desimaalilukua väliltä 0-1
random_list = [random.random() for i in range(5)]
print(random_list)

# Sekoita lista satunnaisesti ja tulosta uusi järjestys
random.shuffle(random_list)
print(random_list)
```

**Esimerkkilähtö:** 

47

[0.2848533746156446, 0.9118379979744344, 0.40169874016049505, 0.01175146335006017, 0.9861229718359766]

[0.9861229718359766, 0.2848533746156446, 0.40169874016049505, 0.9118379979744344, 0.01175146335006017]

## Syvemmälle

Pythonissa satunnaislukuja voidaan luoda käyttämällä sisäänrakennettua `random` -kirjastoa, joka tarjoaa hyödyllisiä toimintoja satunnaislukujen luomiseksi. Yksi suosituimmista on `randint`, joka palauttaa kokonaisluvun halutulta väliltä, mutta myös muita hyödyllisiä toimintoja on, kuten `random`, joka palauttaa float-tyyppisen luvun väliltä 0-1.

Lisäksi on olemassa muita tapoja luoda satunnaislukuja, kuten `uniform`, joka palauttaa antamasi alueen sisältä yhtä todennäköisiä lukuja, tai `gauss`, joka tuottaa Gaussian-jakauman mukaisesti satunnaislukuja.

Satunnaislukujen luominen voi myös olla kätevää, kun testaamme ohjelmia ja haluamme syöttää erilaisia arvoja jokaiselle suorituskerralle. Tai jos haluamme luoda pseudosatunnaisia lukuja, jotka ovat samat jokaisella kerralla ohjelma suoritetaan.

## Katso myös

[Kattava opas Pythonin `random` -kirjastoon](https://www.geeksforgeeks.org/random-module-python/)

[Hyödyllinen esimerkki satunnaislukujen käytöstä Pythonin ohjelmoinnissa](https://realpython.com/python-random/#simulation)

[Muutamia vinkkejä satunnaislukujen käyttöön ja ongelmiin liittyen](https://towardsdatascience.com/mastering-python-random-with-9-simple-tips-448475efca03)