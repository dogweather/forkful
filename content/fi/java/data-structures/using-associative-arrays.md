---
title:                "Assosiatiivisten taulukoiden käyttö"
aliases:
- /fi/java/using-associative-arrays.md
date:                  2024-01-30T19:11:47.006178-07:00
model:                 gpt-4-0125-preview
simple_title:         "Assosiatiivisten taulukoiden käyttö"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Javassa assosiatiiviset taulukot eli mapit mahdollistavat avain-arvo -parien tallentamisen tehokasta datan hakua ja manipulaatiota varten. Ohjelmoijat käyttävät niitä tehtäviin kuten esiintymien laskentaan tai käyttäjien oikeuksien mappaamiseen, koska ne tarjoavat nopean pääsyn ja päivitykset.

## Kuinka:

Java ei sisällä valmiiksi assosiatiivisia taulukoita, kuten jotkin kielet tekevät, mutta se tarjoaa `Map`-rajapinnan ja luokkia, kuten `HashMap` ja `TreeMap`, täyttämään tämän roolin. Tässä on, miten käytetään `HashMap`ia:

```Java
import java.util.HashMap;
import java.util.Map;

public class LearnMaps {
    public static void main(String[] args) {
        // HashMapin luominen
        Map<String, Integer> ageOfFriends = new HashMap<>();
        
        // Alkioiden lisääminen
        ageOfFriends.put("Alice", 24);
        ageOfFriends.put("Bob", 30);
        ageOfFriends.put("Charlie", 28);

        // Alkioiden haku
        System.out.println("Alicen ikä: " + ageOfFriends.get("Alice"));
        
        // Olemattomien avainten käsittely
        System.out.println("Kartalla olemttoman henkilön ikä: " + ageOfFriends.getOrDefault("Dan", -1));

        // Alkioiden iteraatio
        for (Map.Entry<String, Integer> entry : ageOfFriends.entrySet()) {
            System.out.println(entry.getKey() + " on " + entry.getValue() + " vuotta vanha.");
        }
    }
}
```

Esimerkkituloste:

```
Alicen ikä: 24
Kartalla olemttoman henkilön ikä: -1
Alice on 24 vuotta vanha.
Bob on 30 vuotta vanha.
Charlie on 28 vuotta vanha.
```

`HashMap` on vain yksi toteutus. Jos avaimet ovat uniikkeja ja tarvitset ne järjestettyinä, harkitse `TreeMap`ia. Mapille, joka säilyttää lisäysjärjestyksen, `LinkedHashMap` on ystäväsi.

## Syväsukellus

Mapit Javassa ovat osa Collections Frameworkia, joka otettiin käyttöön JDK 1.2:ssa, mutta niitä on paranneltu vuosien varrella merkittävästi. Tähän kuuluu esimerkiksi `forEach`-metodin esittely Java 8:ssa, joka mahdollistaa helpomman iteraation alkioiden yli. Mappien toteutus (`HashMap`, `LinkedHashMap`, `TreeMap`) tulisi määritellä tarpeidesi perusteella järjestyksen ja suorituskyvyn suhteen. Esimerkiksi `HashMap` tarjoaa O(1) aikasuorituskyvyn perusoperaatioille (get ja put), olettaen, että hajautusfunktio jakaa elementit sopivasti ämpäreiden kesken. Kuitenkin, jos tarvitset järjestämistä luonnollisen järjestyksen tai mukautettujen vertailijoiden perusteella, `TreeMap` on valintasi, tarjoten O(log n) ajan lisäykselle ja haulle.

Ennen `Map`in esittelyä assosiatiiviset taulukot toteutettiin yleensä kahdella rinnakkaisella taulukolla (yksi avaimille, toinen arvoille) tai mukautetuilla tietorakenteilla, jotka olivat vähemmän tehokkaita. Nykyiset vaihtoehdot `Map`ille ja sen toteutuksille voivat sisältää kolmannen osapuolen kirjastoja, jotka tarjoavat erikoistuneita mappeja, kuten kaksisuuntaiset mapit (BiMap Googlen Guava-kirjastossa) tapauksissa, joissa tarvitset tehokkaan keinon löytää avaimen arvonsa perusteella. Kuitenkin useimmissa käyttötapauksissa Javassa, standardikirjaston mapit ovat riittävän vankkoja ja joustavia tehtävän hoitamiseen.
