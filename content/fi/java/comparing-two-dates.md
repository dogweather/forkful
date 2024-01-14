---
title:                "Java: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Monissa Java-ohjelmoinnin projekteissa joudutaan vertailemaan kahta päivämäärää, esimerkiksi tarkistaakseen onko jokin tapahtuma jo mennyt tai onko jokin deadline lähestymässä. Tässä blogikirjoituksessa tutustumme siihen, miten nämä vertailut voidaan tehdä helposti ja tarkasti.

## Miten

Vertaaminen kahden päivämäärän välillä onnistuu käyttämällä Java Date-luokan after() ja before() metodeja. Näiden metodien avulla voidaan tarkistaa, onko jokin päivämäärä aikaisempi tai myöhempi toiseen verrattuna.

```java
Date date1 = new Date(2020, 10, 5);
Date date2 = new Date(2020, 10, 10);

// Tarkistetaan, onko date1 myöhempi kuin date2
if(date1.after(date2)){
  System.out.println("date1 on myöhempi kuin date2");
}

// Tarkistetaan, onko date1 aikaisempi kuin date2
if(date1.before(date2)){
  System.out.println("date1 on aikaisempi kuin date2");
}
```

Yllä olevassa esimerkissä luomme kaksi Date-objektia, joille annamme erilaiset päivämäärät. Sen jälkeen käytämme after() ja before() metodeja vertaillaksemme näitä päivämääriä. Kumpikin metodi palauttaa boolean-arvon riippuen siitä, onko vertailu tosi vai epätosi. 

## Syvällisempi sukellus

Jos haluat vertailla päivämääriä tarkemmin, voit käyttää Date-objektin compareTo() metodia. Tämä metodi palauttaa negatiivisen luvun, jos vertailtava päivämäärä on aiempi, nollan jos päivämäärät ovat samat ja positiivisen luvun jos vertailtava päivämäärä on myöhempi.

```java
Date date1 = new Date(2020, 10, 5);
Date date2 = new Date(2020, 10, 10);

// Vertaillaan date1 ja date2 päivämääriä
int result = date1.compareTo(date2);

// Tulostetaan tuloksen mukaan
if(result < 0){
  System.out.println("date1 on aiempi kuin date2");
}else if(result == 0){
  System.out.println("date1 ja date2 ovat samat");
}else{
  System.out.println("date1 on myöhempi kuin date2");
}
```

Tämä metodi tarjoaa tarkemman vertailun päivämäärien välillä, mahdollistaen myös päivämäärien vertailun sekunnin ja millisekunnin tarkkuudella.

## Katso myös

- [Java Date-luokan dokumentaatio (englanniksi)](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Tietoa päivämäärien vertailusta Javassa (englanniksi)](https://www.geeksforgeeks.org/compare-dates-java/)