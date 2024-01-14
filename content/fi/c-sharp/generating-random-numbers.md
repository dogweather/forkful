---
title:                "C#: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi?

Satunnaisilla numeroilla on monia käyttötarkoituksia ohjelmoinnissa, kuten pelien kehittäminen, satunnaisten käyttäjätunnusten tai salasanojen luominen ja tietojen salaaminen. Ne voivat myös olla hyödyllisiä testattaessa ohjelmistojen toimintaa erilaisilla syötteillä.

## Miten?

C#:ssa on valmiina toiminto satunnaisluvun generoimiseksi. Tämän toiminnon nimi on "Random" ja se löytyy "System" nimisestä nimiavaruudesta. Käytämme sitä seuraavalla tavalla:

```C#
Random rnd = new Random();
int randomNumero = rnd.Next(1,100);
Console.WriteLine($"Satunnainen numero väliltä 1-100: {randomNumero}");
```
Tämä koodi luo uuden Random-olion ja kutsuu sen Next-metodia, joka ottaa kaksi parametria. Ensimmäinen parametri määrittelee alarajan ja toinen ylärajan, joiden väliltä satunnaisluku generoidaan. Tässä esimerkissä saatamme saada esimerkiksi luvun 42. Voimme myös käyttää toista Next-metodin versiota, joka palauttaa desimaalilukuja.

On tärkeää huomata, että Random-olion luoma satunnaisuus perustuu koneen kellonaikaan. Tämä tarkoittaa sitä, että mikäli satunnaisluku tarvitaan useaan kertaan hyvin lyhyen ajan sisällä, saatamme saada saman luvun useita kertoja. Tätä voidaan välttää siirtämällä Random-olion luonti ohjelman alkuun tai luomalla se staattisesti, esimerkiksi luokan sisällä. Näin varmistetaan, että sama satunnaisluku ei toistu.

## Syvällisempi sukellus

Random-luokassa on myös muita metodeja, kuten NextBytes ja NextDouble. NextBytes-metodi täyttää annetun taulukon satunnaisilla tavuilla ja NextDouble palauttaa satunnaisen desimaaliluvun väliltä 0 ja 1. Voit myös määrittää Random-olion siemenen, jolloin saat aina saman sarjan satunnaislukuja.

On myös tärkeää olla varovainen käytettäessä Random-luokkaa tietoturvallisuuteen liittyvissä tarkoituksissa. Random-luokka ei luo todellista satunnaisuutta, vaan se perustuu matemaattisiin algoritmeihin. Tästä syystä sitä ei suositella esimerkiksi salasanojen luomiseen.

## Katso myös
- C# Random-luokan virallinen dokumentaatio: https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netframework-4.8
- Bensu blogi, Miten ja miksi käyttää satunnaisia numeroita ohjelmoinnissa: https://www.bensu.com/blogi/miten-ja-miksi-kayttaa-satunnaisia-numeroita-ohjelmoinnissa/
- Codecademy, Kuinka käyttää satunnaislukuja C#-kielisessä ohjelmoinnissa: https://www.codecademy.com/articles/using-random-in-C%23