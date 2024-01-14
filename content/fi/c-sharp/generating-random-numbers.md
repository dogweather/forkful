---
title:    "C#: Sattumanvaraisten lukujen luominen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi
On monia syitä, miksi haluat ehkä luoda satunnaisia numeroita ohjelmointisi aikana. Ehkä tarvitset niitä testauksia varten, luodaksesi erilaisia käyttäjätunnuksia tai salasanoja, tai ehkä haluat pelissä tai sovelluksessa käyttää satunnaisia elementtejä, kuten vihollisia tai kohteita.

## Miten tehdä se
Käytä C# -kielen Random-luokkaa luodaksesi satunnaisia numeroita. Voit alustaa Random-luokan eri tavoilla, esimerkiksi käyttää nykyistä kellonaikaa tai antaa sille tietyn siemenarvon. Tämän jälkeen voit käyttää sen Next() -metodia saadaksesi satunnaisen kokonaisluvun tai NextDouble() -metodia saadaksesi satunnaisen desimaaliluvun. Katso alla esimerkki, jossa luodaan 5 satunnaista kokonaislukua välillä 1-10:

```C#
Random random = new Random();
for (int i = 0; i < 5; i++)
{
    int randomNumber = random.Next(1, 11);
    Console.WriteLine(randomNumber);
}
```
Esimerkki tulostaa seuraavan:

```
8
3
10
5
1
```
Voit myös käyttää Random-luokan muita metodeja, kuten NextBytes() saadaksesi satunnaisen tavumäärän tai NextBytes() saadaksesi satunnaisen luvun tietyllä alueella.

## Syvempi sukellus
Random-luokka käyttää Mersenne Twister -algoritmia, joka on yksi parhaiten tutkituista ja tehokkaimmista satunnaisuusgeneraattoreista. Se pystyy tuottamaan suuria määriä satunnaisia lukuja nopeasti ilman toistuvuutta. Tämä tekee siitä ihanteellisen valinnan ohjelmointiprojekteihin, joissa tarvitaan satunnaisia lukuja.

## Katso myös
- [Microsoftin virallinen dokumentaatio Random-luokasta](https://docs.microsoft.com/fi-fi/dotnet/api/system.random?view=net-5.0)
- [Satunnaislukujen luominen C# -koodilla](https://www.c-sharpcorner.com/article/random-number-in-c-sharp/)
- [Mersenne Twister -algoritmin selitys](https://en.wikipedia.org/wiki/Mersenne_Twister)