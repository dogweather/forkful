---
title:    "Bash: Tiedoston lukeminen"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi

Blogikirjoituksemme aiheena on Bash-ohjelmointi ja kuinka lukea tiedostoa käyttäen Bash-kieltä. On monia syitä, miksi kannattaa tutustua tähän taitoon. Yksinkertaisimmillaan tiedostojen lukeminen Bashilla helpottaa tiedostojen käsittelyä ja muokkaamista, joten se voi olla hyödyllistä monenlaisissa projekteissa.

## Kuinka

Bash tarjoaa erinomaisen tavan lukea tekstiä tiedostosta ja käsitellä sitä haluamallamme tavalla. Käytämme esimerkkinä yksinkertaista tekstitiedostoa, jossa on rivejä ja jokaisella rivillä on yksi sana.

```Bash
while read word; do
  echo "Sana: $word"
done < tiedosto.txt
```

Yllä oleva koodinpätkä lukee tiedoston "tiedosto.txt" rivejä ja tulostaa jokaisen rivin sisältämän sanan muodossa "Sana: [rivin sana]". Tämä on vain yksi esimerkki, Bashilla on monia muita tapoja käsitellä tekstiä ja tiedostoja. Voit kokeilla erilaisia komentoja ja koodinpätkiä, ja näet miten tiedoston lukeminen toimii eri tavoin.

## Syventyvä sukellus

Tiedoston lukeminen Bashilla onnistuu myös monimutkaisempien tiedostojen kanssa. Voit esimerkiksi käyttää erilaisia ehtolausekkeita ja silmukoita lukemisen yhteydessä, jolloin voit suodattaa ja muokata tiedostoa entistä tarkemmin. Lisäksi voit lukea ja käsitellä myös muita tiedostomuotoja, kuten CSV-tiedostoja.

## Katso myös

- [Bash manuaali (suomeksi)](https://www.manpagez.com/man/1/bash/)
- [Bash käyttöopas (englanniksi)](https://www.gnu.org/software/bash/manual/bash.txt)
- [Bash Scripting Tutorial (englanniksi)](https://linuxconfig.org/bash-scripting-tutorial)
- [Pätkäpankki - Bash ohjelmoinnin palvelimilla (suomeksi)](https://paxli.fi/archives/2041)