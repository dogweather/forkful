---
title:    "Java: Päivämäärän muuntaminen merkkijonoksi."
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi

Jokaisella Java-ohjelmoijalla on joskus ollut tarve muuttaa päivämäärä merkkijonoksi. Tämä voi olla tarpeen esimerkiksi tulostettaessa päivämäärää käyttäjälle tai tallennettaessa sitä tietokantaan. Tässä blogikirjoituksessa näytän, miten tämä onnistuu helposti Java-kielellä.

## Miten

Muuttaaksesi päivämäärän merkkijonoksi Java-kielellä, tarvitset kaksi asiaa: päivämäärä-objektin ja muotoilun. Voit luoda päivämäärä-objektin esimerkiksi käyttämällä `Date`-luokkaa. Muotoilun voit määrittää haluamallasi tavalla käyttämällä `SimpleDateFormat`-luokkaa. Käyttämällä näitä kahta yhdessä, voit muuttaa päivämäärän helposti merkkijonoksi.

Esimerkiksi, jos haluamme muuttaa nykyisen päivämäärän merkkijonoksi esimerkiksi muodossa "dd/MM/yyyy", voimme käyttää seuraavaa koodia:

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateToStringExample {

  public static void main(String[] args) {
    Date date = new Date();
    SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
    String dateString = dateFormat.format(date);
    System.out.println(dateString);
  }
}
```

Tämän koodin tulostus olisi esimerkiksi "05/05/2021", jos päivämäärä on toukokuun 5. päivä vuonna 2021.

On tärkeää huomata, että muotoilun merkkijonoa voidaan muokata haluamallasi tavalla. Esimerkiksi "dd/MMM/yyyy" muotoilu tulostaisi kuukauden lyhenteen sijaan numeron.

Käyttämällä tätä metodia, voit muuttaa päivämäärän haluamaasi muotoon ja tulostaa sen esimerkiksi konsoliin tai tallentaa sen tietokantaan.

## Syvempi sukellus

On tärkeää huomata, että `Date`-luokka on vanhentunut ja sitä ei suositella käytettäväksi nykypäivän Java-sovelluksissa. Sen sijaan suositellaan käyttämään uudempia `java.time`-luokkia, kuten `LocalDate` ja `DateTimeFormatter`. Nämä uudemmat luokat tarjoavat paremman tavan hallita päivämääriä ja niiden muotoilua.

Esimerkiksi, jos haluamme käyttää samoja muotoiluja kuin edellisessä esimerkissä, voimme käyttää seuraavaa koodia:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateToStringExample {

  public static void main(String[] args) {
    LocalDate date = LocalDate.now();
    DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern("dd/MM/yyyy");
    String dateString = dateFormat.format(date);
    System.out.println(dateString);
  }
}
```

Tulostus olisi tässäkin tapauksessa "05/05/2021". Kuten näet, uudempi tapa käsitellä päivämääriä on selkeämpi ja helpommin ylläpidettävä.

## Katso myös

- [Java Date Class](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java SimpleDateFormat Class](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java LocalDate Class](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java DateTimeFormatter Class](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)