---
title:    "Java: Päivämäärän hakeminen"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi

Tietojenkäsittelyssä on useita tilanteita, joissa haluamme käyttää tietokoneen nykyistä päivämäärää. Esimerkiksi järjestelmä voi tarvita päivämäärää päiväyksen tallentamiseen tietokantaan tai käyttäjälle näytettävään viestiin. Java-ohjelmoinnissa käytetään Date-luokkaa nykyisen päivämäärän hankkimiseen.

## Miten

Nykyisen päivämäärän saaminen Java-ohjelmassa on helppoa. Käytämme vain Date-luokan getInstance-metodia, joka luo uuden Date-olion nykyisellä päivämäärällä ja ajalla.

```Java
Date nykyinenPaivamaara = Date.getInstance();
System.out.println(nykyinenPaivamaara);
```

Tämän koodin tuloste riippuu ajankohdasta, jolloin se suoritetaan. Esimerkiksi 18. lokakuuta 2021 se voi tulostaa "Mon Oct 18 15:41:23 UTC 2021".

Voimme myös muuttaa Date-olion muotoa haluamallamme tavalla käyttämällä SimpleDateFormat-luokkaa. Seuraava esimerkki esittää päivämäärän muuntamisen suomalaiseen muotoon.

```Java
SimpleDateFormat muotoilija = new SimpleDateFormat("dd.MM.yyyy");
String paivamaara = muotoilija.format(nykyinenPaivamaara);
System.out.println(päivämäärä);
```

Tämä koodi tulostaa "18.10.2021".

## Syvällisempi sukellus

Date-luokka sisältää myös muita hyödyllisiä metodeja, kuten before(), after() ja compareTo(), jotka mahdollistavat päivämäärien vertailun. Ne ovat hyödyllisiä esimerkiksi, kun haluamme tarkistaa onko tietty päivämäärä ennen vai jälkeen toisen.

On myös tärkeää huomata, että Java 8:ssa otettiin käyttöön uusi LocalDate-luokka, joka tarjoaa entistä monipuolisemman päivämäärän käsittelyn. Se lisäksi että se pystyy käsittelemään päivämääriä, siihen on myös lisätty päivämäärien ja ajankohtien välinen ajan leima.

## Katso myös

- [Java Date-luokka (Oracle Docs)](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java SimpleDateFormat-luokka (Oracle Docs)](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java LocalDate-luokka (Oracle Docs)](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)