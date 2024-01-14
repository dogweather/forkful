---
title:                "Bash: Tiedoston lukeminen"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

Hei lukijat!

Miksi lukisi tekstitiedostoa?

On monia syitä, miksi voit haluta lukea tekstitiedostoa Bash-ohjelmoinnissa. Tekstitiedostoja käytetään usein tallentamaan tietoja tai tekemään muutoksia olemassa oleviin tiedostoihin. Niitä voidaan myös käyttää ohjelmien syötteiden lukemiseen tai tulostusten tallentamiseen. Lukemalla tekstitiedoston, voit tarkastella sen sisältöä ja käyttää sitä osana koodiasi.

Miten:

Voit lukea tekstitiedoston Bash-ohjelmalla käyttämällä "cat" -komentoa. Tämä komento näyttää tiedoston sisällön suoraan komentorivillä. Voit myös tallentaa tiedoston sisällön muuttujaan käyttämällä "read" -komentoa ja määrittämällä tiedoston nimen. Sitten voit käyttää muuttujaa osana koodiasi.

Esimerkiksi:

```Bash
cat tiedosto.txt
read sisalto < tiedosto.txt
echo $sisalto
```

Tässä esimerkissä ensimmäinen komento tulostaa tiedoston sisällön suoraan komentoriville. Toinen komento tallentaa tiedoston sisällön muuttujaan nimeltä "sisalto" ja kolmas komento tulostaa muuttujan arvon, joka on tiedoston sisältö.

Voit myös lukea tekstitiedoston käyttämällä while-silmukkaa yhdessä "read" -komennon kanssa. Tämä on hyödyllistä, jos haluat käsitellä tiedoston sisältöä rivi kerrallaan.

Esimerkiksi:

```Bash
while read rivi; do
echo $rivi
done < tiedosto.txt
```

Tässä while-silmukassa jokainen tiedoston rivi tallennetaan muuttujaan nimeltä "rivi" ja tulostetaan sitten komentoriville.

Syväsukellus:

Tekstitiedoston lukeminen voi olla hyödyllistä, kun haluat käyttää olemassa olevia tietoja ohjelmassasi. Voit myös käyttää muita komentoja, kuten "grep" tai "sed", etsiäksesi tiettyä dataa tiedostosta tai muokataksesi tiedoston sisältöä. Voit myös käyttää ehtolauseita, kuten if-lauseita tai case-lauseita, käsitelläksesi tiedoston sisältöä haluamallasi tavalla.

Muista aina käsitellä tekstitiedostoja varovaisesti ja varmista, että tiedoston oikeudet ovat asetettu oikein suojataksesi tärkeitä tietoja.

Nähdään pian:

Katso myös näitä artikkeleita oppiaksesi lisää Bash-ohjelmoinnista ja tekstitiedoston lukemisesta:

- [Bash-skriptaus oppaaseen](https://www.bash-skriptaus.fi/)
- [Linuxin peruskomennot](https://www.linuxperuskomennot.fi/tiedoston_kayttaminen/)
- [Bashin while-loop esimerkkejä](https://www.cyberciti.biz/faq/bash-while-loop/)
- [Lisätietoja Bashin komentorivistä](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)