---
title:                "Java: Virheenjäljitystulostus"
programming_language: "Java"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmointiprojektin aikana esiintyy erilaisia virheitä ja bugeja, jotka voivat vaikeuttaa koodin toimintaa. Tässä tilanteessa voi olla hyödyllistä ottaa debuggaus käyttöön ja tulostaa koodin eri vaiheissa tietoa suorituksen kulusta ja muuttujien arvoista, jotta virheiden etsiminen olisi helpompaa. Tässä blogikirjoituksessa käymme läpi, miten debuggausta ja tulostusta voidaan hyödyntää Java-ohjelmoinnissa.

## Kuinka

Debuggausta ja tulostusta voidaan käyttää Java-ohjelmoinnissa hyödyllisenä työkaluna virheiden etsimisessä. 
Alla on esimerkki yksinkertaisesta Java-ohjelmasta, joka tulostaa merkkijonon "Hello World" ja sen jälkeen suorittaa laskutoimituksen ja tulostaa sen tuloksen:

```Java
public class TulostusEsimerkki {

    public static void main(String[] args) {
        System.out.println("Hello World");
        
        int x = 5;
        int y = 7;
        int summa = x + y;
        
        System.out.println("Summa: " + summa);
    }
}
```

Tämän koodin ajamisen jälkeen konsolille tulostuu seuraava tieto:

```
Hello World
Summa: 12
```

Vaikka tässä esimerkissä ei esiinny mitään virheitä, voimme kuitenkin käyttää tulostusta hyödyksi selvitellessämme virheitä. Esimerkiksi jos laskutoimituksessa ilmenee jokin virhe, voimme tulostaa muuttujien arvot ennen laskutoimitusta ja sen jälkeen, jotta voimme tarkistaa, missä vaiheessa virhe tapahtuu. Lisäksi voimme myös käyttää tulostusta seuraamaan ohjelman suorituksen kulkua ja varmistamaan, että ohjelma suorittaa halutut toiminnot oikeassa järjestyksessä.

## Syvällinen sukellus

Java tarjoaa erilaisia tapoja tulostaa debug tietoa. Yksi tapa on käyttää `System.out.println()`-metodia, joka tulostaa annetun syötteen konsoliin. Tämän lisäksi on myös mahdollista käyttää `System.out.format()`-metodia, joka toimii samalla tavalla kuin `printf` C-ohjelmointikielessä.

Esimerkiksi alla oleva koodi tulostaa konsoliin kolme merkkijonoa, joista jokainen on tulostettu omalle rivilleen:

```Java
String etunimi = "Mikko";
String sukunimi = "Maasalo";
int ika = 25;

System.out.println("Etunimi: " + etunimi);
System.out.println("Sukunimi: " + sukunimi);
System.out.println("Ikä: " + ika);
```

Tulostuksena tähän koodiin saamme seuraavan:

```
Etunimi: Mikko
Sukunimi: Maasalo
Ikä: 25
```

Voimme myös hyödyntää `System.out.format()`-metodia, joka toimii samalla tavalla kuin `printf` C-ohjelmointikielessä. Tässä on esimerkki samasta koodista käyttäen `System.out.format()`-metodia:

```Java
String etunimi = "Mikko";
String sukunimi = "Maasalo";
int ika = 25;

System.out.format("Etunimi: %s%n", etunimi);
System.out.format("Sukunimi: %s%n", sukunimi);
System.out.format("Ikä: %d%n", ika);
```

Tulostus tähän on sama kuin edellisessä esimerkiss