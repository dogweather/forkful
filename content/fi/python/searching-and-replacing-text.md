---
title:    "Python: Tekstin etsiminen ja korvaaminen"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi

Ai, miksi kukaan haluaisi vaivautua tekstien hakemiseen ja korvaamiseen? No, yksinkertaisesti sanottuna tämä yksi pieni toiminto voi säästää valtavasti aikaa ja vaivaa, kun työskennellään suurten tekstimäärien kanssa. Ajattele esimerkiksi, jos sinun tarvitsee muuttaa saman sanan tai lauseen useammasta dokumentista tai kooditiedostosta. Se olisi melko tylsää ja aikaavievää tehdä manuaalisesti, mutta tekstien hakemisen ja korvaamisen avulla voit tehdä sen helposti ja nopeasti!

## Kuinka tehdä tekstien haku ja korvaaminen Pythonilla

Aloitetaan yksinkertaisella esimerkillä, jossa muutamme vanhan yrityksen nimen uudeksi. Käytämme tässä esimerkissä Pythonin `replace()` -funktiota, jonka avulla voimme etsiä ja korvata tekstiä annetuista merkkijonoista.

*Esimerkkikoodi:*

```Python
vanha_nimi = "Vanha jäätelötehdas"
uusi_nimi = vanha_nimi.replace("Vanha", "Uusi")

print(uusi_nimi)
```

*Tuloste:*

`Uusi jäätelötehdas`

Yllä olevassa koodiesimerkissä ensin määritämme muuttujan `vanha_nimi`, joka sisältää vanhan yrityksen nimen. Sitten käytämme `replace()` -funktiota ja annamme sille kaksi parametria: etsittävän merkkijonon ja korvaavan merkkijonon. Tulosteessa näemme, että uusi nimi on korvattu vanhan nimen kanssa.

Mutta mitä jos haluamme muuttaa useamman kuin yhden sanan? Voimme käyttää samaa `replace()` -funktiota esimerkiksi muuttaaksemme kaikki huonoja käytäntöjä sisältävät dokumentit "huonoista käytännöistä" "hyviksi käytännöiksi".

*Esimerkkikoodi:*

```Python
dokumentti = "Tämä dokumentti sisältää paljon huonoja käytäntöjä, jotka on muutettava hyviksi käytännöiksi."
uusi_dokumentti = dokumentti.replace("huonoja käytäntöjä", "hyviä käytäntöjä")

print(uusi_dokumentti)
```

*Tuloste:*

`Tämä dokumentti sisältää paljon hyviä käytäntöjä, jotka on muutettava hyviksi käytännöiksi.`

Hienoa! Huomaat varmasti, kuinka nopeasti voimme muuttaa useita merkkijonoja kerralla.

## Syväsukellus

Nämä ovat vain muutamia esimerkkejä tekstien hakemisesta ja korvaamisesta Pythonilla. Mutta Pythonilla on paljon muutakin tarjottavaa tähän toimintoon liittyen. Voit esimerkiksi käyttää regular expression -kirjastoa (`re`) monimutkaisempien haku- ja korvaustoimintojen suorittamiseen. Voit myös käyttää `sub()` -funktiota, joka antaa sinun määrittää korvaussäännöt samassa muodossa kuin Pythonin `dictionary`-rakenne.

Voit myös käyttää tekstieditorityökaluja, kuten Sublime Text ja Notepad++, joilla on sisäänrakennettuja toimintoja tekstien hakemiseen ja korvaamiseen. Monissa tekstieditoreissa voit myös käyttää regular expressioneja näissä toiminnoissa.

Joten on selvää, että tekstien hakeminen ja korvaaminen Pythonilla on erittäin hyödyllinen taito ja sen avulla voit