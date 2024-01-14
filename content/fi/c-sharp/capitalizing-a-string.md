---
title:    "C#: Merkkijonon muuttaminen isoin kirjaimin"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit ylipäänsä käyttää aikaa pienen tehtävän kuten merkkijonon muokkaamisen kokoamiseen? Yhtenä mahdollisuutena on, että haluat pitää koodisi selkeänä ja järjestelmällisenä. Joskus pienikin muutos voi tehdä koodistasi helpommin luettavan ja ymmärrettävän, mikä tekee sinun työskentelystäsi tehokkaampaa.

## Kuinka

Onneksi C#:ssa on sisäänrakennettu metodi, joka muuttaa merkkijonon ensimmäiset kirjaimet isoiksi kirjaimiksi. Tämä tarkoittaa sitä, että voit helposti käyttää tätä metodia muokkaamalla merkkijonoa haluamallasi tavalla.

```.
C# 
string s = "esimerkki merkkijonosta";
s = s.ToUpper();
Console.WriteLine(s);
```

Tämä koodi palauttaa "ESIMERKKI MERKKIJONOSTA" konsoliin. Kuten näet, metodin käyttäminen on hyvin yksinkertaista.

Yksi tärkeä asia huomioitavaksi on, että tämä metodi muuttaa ainoastaan ensimmäisen kirjaimen kooltaan, eli jos haluat muuttaa koko merkkijonon muotoa, sinun tulee käyttää muita metodeja tai kirjoittaa oma koodisi.

## Syvällisempi sukellus

Miksi haluaisit muuttaa merkkijonon kirjaimien kokoa? Yksi mahdollinen syy on, että saat tietoja käyttäjiltä, jotka eivät aina huomaa kirjoittaa kaikki kirjaimet isolla. Tällöin on helpompaa muuttaa merkkijonon ensimmäinen kirjain isoksi, jotta lopputulos olisi kaikilta osin yhtenäinen ja helposti luettava.

## Katso myös

- [Microsoftin virallinen dokumentaatio merkkijonon muokkaamiseen C#](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netframework-4.8)
- [C# merkkijonojen perusteet](https://www.tutorialspoint.com/csharp/csharp_strings.htm)
- [Vinkkejä koodin siistimiseen C#:ssa](https://www.codeproject.com/Articles/1117530/Tips-to-keep-your-csharp-code-clean)