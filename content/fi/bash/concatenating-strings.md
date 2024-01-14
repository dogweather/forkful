---
title:    "Bash: Merkkijonojen yhdistäminen"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi?

Ennen kuin aloitamme puhumaan merkkijonojen yhdistämisestä Bash-ohjelmoinnissa, on tärkeää ymmärtää, miksi se on hyödyllistä. Merkkijonot ovat perustavanlaatuinen osa ohjelmointia ja ne ovat tärkeitä muun muassa tietojen käsittelyssä ja tiedostojen hallinnassa. Yhdistämällä merkkijonoja voimme helposti luoda uusia sanallisia ilmauksia ja tehdä monimutkaisia muutoksia olemassa oleviin merkkijonoihin.

## Miten?

Merkkijonojen yhdistäminen Bash-ohjelmoinnissa on helppoa. Voimme käyttää joko pisteoperaattoria tai liitännäisoperaattoria (+). Pisteoperaattori yhdistää kaksi merkkijonoa yhdeksi, kun taas liitännäisoperaattori lisää merkkijonon perään toisen merkkijonon.

```Bash
#Pisteoperaattori
string1="Hei"
string2="maailma"
echo $string1$string2 #Tulostaa "Heimaailma"

#Liitännäisoperaattori
string="Tervetuloa"
greeting="!"
echo $string$greeting #Tulostaa "Tervetuloa!"
```

## Syvemmälle

Bashilla on myös mahdollista yhdistää merkkijonoja muuttujien kanssa. Tämä on hyödyllistä, kun haluamme luoda joustavia ja muokattavia merkkijonoja. Voimme käyttää muuttujia pisteoperaattorin tai liitännäisoperaattorin kanssa tai käyttää muotoiltua tulostusta.

```Bash
#Pisteoperaattori muuttujan kanssa
user="John"
greeting="Hei"
echo $greeting $user #Tulostaa "Hei John"

#Liitännäisoperaattori muuttujan kanssa
name="Maria"
age="25"
echo "Minun nimeni on $name ja olen $age vuotta vanha." #Tulostaa "Minun nimeni on Maria ja olen 25 vuotta vanha."

#Muotoiltu tulostus
country="Suomi"
capital="Helsinki"
printf "Maan nimi on %s ja pääkaupunki on %s." $country $capital #Tulostaa "Maan nimi on Suomi ja pääkaupunki on Helsinki."
```

## Katso myös

Tässä artikkelissa käsiteltiin perusteita merkkijonojen yhdistämisestä Bash-ohjelmoinnissa. Jatkamalla opiskelua näistä peruselementeistä, voit luoda monimutkaisia ohjelmia, jotka käsittelevät merkkijonoja ja tiedostoja. Lisätietoa Bash-ohjelmoinnista ja sen käyttämistä merkkijonojen käsittelyssä voit löytää seuraavista resursseista:

[Merkkijonojen käsittely Bashissa (Linux Hint)](https://linuxhint.com/processing-strings-bash/)

[Muuttujat ja muotoiltu tulostus Bashissa (The Linux Documentation Project)](https://tldp.org/LDP/abs/html/varsubn.html)

[Bash-ohjelmoinnin perusteet (The Linux Documentation Project)](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)