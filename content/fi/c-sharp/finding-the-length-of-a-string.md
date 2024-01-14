---
title:    "C#: Merkkijonon pituuden etsiminen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi: Miksi löytäisit merkkijonon pituuden?

Olipa sitten koodaaja aloittelija tai kokenut ohjelmistosuunnittelija, merkkijonon pituuden löytäminen on yksi perustaidoista, jotka jokaisen tulisi hallita. Merkkijonot ovat luultavasti yksi yleisimmistä tietotyypeistä, joita käsittelemme ohjelmointikielessä, joten tietääksesi niiden pituuden voi auttaa sinua helpottamaan ja nopeuttamaan koodaamista.

## Kuinka tehdä: Esimerkkiä koodauksella ja tulosteella

Merkkijonojen pituuden löytäminen C#-ohjelmointikielellä on hyvin helppoa. Käytännössä merkkijonon pituus on vain sen merkkien lukumäärä. Voit käyttää "Length" -ominaisuutta selventämään tätä.

```C#
string merkkijono = "Tämä on vain esimerkki";
Console.WriteLine(merkkijono.Length);
```

Tämä palauttaa seuraavan tulosteen:

```
24
```

Huomaa, että välilyönnitkin lasketaan merkkijonon pituuteen, joten pelkkä sanan "vain" pituus onkin 5, sen lisäksi että siinä on välilyöntiä.

Voit myös yhdistellä merkkijonoja ja laskutoimituksia löytämään esimerkiksi kahden merkkijonon pituuden summan. Seuraavassa esimerkissä yhdistetään kaksi merkkijonoa ja lasketaan sitten niiden yhteispituus:

```C#
string ensimmainen = "Tämä on ";
string toinen = "toinen esimerkki";
Console.WriteLine(ensimmainen.Length + toinen.Length);
```

Tulosteen pitäisi olla:

```
21
```

Tässä tapauksessa tuloksena on 21, koska molemmissa merkkijonoissa on myös välilyöntejä.

## Syvällisempi tarkastelu: Merkkijonojen pituuden löytäminen

Merkkijonot ovat C#-ohjelmointikielessä taulukoita, joten voidaan ajatella, että niiden pituus kertoo taulukon solujen lukumäärän. Jokainen merkki tässä taulukossa on indeksoitu, joten voit päästä käsiksi mihin tahansa yksittäiseen merkkiin merkkijonossa käyttämällä sen indeksiä. Esimerkiksi merkkijonon "Moi" indeksit olisivat 0, 1 ja 2.

Merkkijonon pituuden löytäminen on myös tärkeää tietojen käsittelyssä, sillä voimme esimerkiksi tarkistaa, onko merkkijono tarpeeksi pitkä ennen sen käyttämistä. Tämä auttaa meitä löytämään mahdolliset virheet tai bugit koodistamme.

## Katso myös

- [Microsoftin virallinen dokumentaatio merkkijonon pituuden löytämisestä C#-ohjelmointikielessä](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/#getting-string-properties)
- [The C# Players Guide](http://www.csharpprogrammingguide.com/ch6-strings.php)
- [C#-ohjelmointi mielenkiintoisen esimerkin avulla: Merkkijonojen pituus ja yhdistäminen](https://www.geeksforgeeks.org/c-sharp-programming-excercises-string-length-concat-function/)