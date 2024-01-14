---
title:    "Fish Shell: Kirjoittaminen käsittelyvirheelle"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi
Jos olet ohjelmoija Fish Shellin käyttäjä, saatat huomata, että olet aika ajoin kirjoittanut ">>" ennen komentojasi. Tässä blogikirjoituksessa tarkastelemme tarkemmin, miksi kirjoittaminen standardivirheeseen voi olla hyödyllistä ja miten se tehdään.

## Miten
Fish Shellin avulla voit helposti kirjoittaa komentoja standardivirheeseen käyttämällä "echo" -komennon "e" -vaihtoehtoa. Esimerkiksi:

```Fish Shell
echo -e "Tervetuloa Fish Shellin maailmaan!" 2>&1
```

Tämä lauseke antaa sinulle saman tulosteen kuin tavallinen "echo" -komennon käyttö, mutta se kirjoitetaan nyt myös standardivirheeseen. Tulosteen pitäisi näyttää suunnilleen tältä:

```Fish Shell
Tervetuloa Fish Shellin maailmaan!
```

## Syvemmälle
Kirjoittaminen standardivirheeseen voi olla hyödyllistä silloin, kun haluat tulostaa virheilmoituksia tai muita tärkeitä viestejä, jotka on tarkoitus huomata. Standardivirheeseen kirjoittaminen erottuu standarditulosteesta, joten se voi auttaa tunnistamaan ongelmakohtia ja virheitä nopeasti.

Jos haluat kirjoittaa vain standardivirheeseen ilman tavallista tulostetta, voit käyttää ">>&" -merkintää. Esimerkiksi:

```Fish Shell
echo "Tämä ei näy tavallisessa tulosteessa" >>&2
```

Tämä antaa sinulle seuraavan tulosteen:

```Fish Shell
Tämä ei näy tavallisessa tulosteessa
```

## Katso myös
- [Fish Shell Documentaatio - echo](https://fishshell.com/docs/current/cmds/echo.html)
- [Fish Shell Ohjeet - Redirection](https://fishshell.com/docs/current/tutorial.html#redirection)
- [Linux Journey - Standard Streams](https://linuxjourney.com/lesson/standard-streams)