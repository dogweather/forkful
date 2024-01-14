---
title:                "C#: Alimerkkijonojen erottelu"
simple_title:         "Alimerkkijonojen erottelu"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi 

Substringien erottaminen on tärkeä osa ohjelmointia, joka sallii meidän käsitellä tekstipohjaisia tietoja osina. Tämä on hyödyllistä silloin kun haluamme etsiä tiettyjä tietoja tekstistä tai muokata sitä eri tavalla. Tässä blogitekstissä käymme läpi, miten voit helposti erottaa substrings C# -ohjelmointikielessä. 

## Miten 

Erota substringi tekstistä käyttäen ```Substring``` -metodia C# :ssa. Tämä metodi ottaa kaksi parametria: aloitusindeksin ja pituuden. 

Esimerkiksi, jos haluat erottaa sanan "C# ohjelmointi" lauseesta "Pidän C# ohjelmoinnista", käytä seuraavaa koodia:

```C# 
string lause = "Pidän C# ohjelmoinnista";
string ohjelmointi = lause.Substring(5, 13);
Console.WriteLine(ohjelmointi);
```

Tämä tulostaa "C# ohjelmointi" konsoliin. Aloitimme erottamaan sanan indeksistä 5 (lauseen 6. merkki, huom. indeksit alkavat 0:sta) ja kertomalla pituuden, eli 13 merkkiä. Voit myös käyttää ```Substring``` -metodia muuttujille, joita et muokkaa, esimerkiksi:

```C#
string yhteys = "www.tämäonlinkki.com"
string linkki = yhteys.Substring(4, 18);
Console.WriteLine(linkki);
```

Tämä tulostaa "tämäonlinkki.com". 

## Syvä Sukellus 

```Substring``` -metodin lisäksi C# sisältää myös muita hyödyllisiä funktioita tekstien käsittelyyn, kuten ```IndexOf```, joka palauttaa ensimmäisen esiintymän indeksin annetusta merkkijonosta. Tämä on erittäin hyödyllistä silloin kun haluamme tietää tietyn sanan sijainnin lauseessa.

```
string lause = "Tämä teksti on esimerkki";
int sijainti = lause.IndexOf("teksti");
Console.WriteLine(sijainti);
```

Tämä tulostaa "5" konsoliin, koska sana "teksti" alkaa lauseessa kohdasta 5. Voit myös käyttää ```Substring``` ja ```IndexOf``` yhdessä, jolloin voit erottaa sanan mistä tahansa kohdasta lauseessa.

On myös tärkeää huomata, että C#-kieli tukee Unicodea, joten voit käyttää erikoismerkkejä ja eri kieliä substringeissä ilman ongelmia. 

## Katso myös 

- Microsoftin C# dokumentaatio: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/
- W3C tutorial: https://www.w3schools.com/cs/cs_strings_substrings.asp
- Tutorialspoint tutorial: https://www.tutorialspoint.com/csharp/csharp_strings.htm