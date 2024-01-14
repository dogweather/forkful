---
title:                "C#: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Jokaiselle koodaajalle tulee vastaan tilanne, jossa haluat muuttaa saman tekstin esiintymän kaikkien paikkojen tietynlaiseksi. Se voi olla kirjoitusvirheen korjaus, tekstin muokkaaminen tai koodin refaktorointi. Tässä tapauksessa tarvitset tekstinsyötön hakutoiminnallisuuden, joka pystyy muuttamaan vain tietyn tekstikuvion esiintymät.

## Kuinka

C#:n Replace-metodi on täydellinen työkalu tämän tekemiseen. Se on osa string-tyyppiä ja se tarjoaa kätevän tavan korvata teksti tietyn kuvion kanssa. Voit käyttää sitä seuraavasti:

```
string teksti = "Tämä on esimerkki.";
string uusiTeksti = teksti.Replace("esimerkki", "malli");
```

Tämä koodi korvaa kaikki "esimerkki" -tekstin esiintymät "malli" -tekstillä ja uusiTeksti-muuttujassa on lopputulos, joka on "Tämä on malli." 

Voit myös käyttää Replace-metodia yhdessä IndexOf-metodin kanssa, jos haluat korvata vain tietyn esiintymän. Voit tehdä sen näin:

```
string teksti = "Olen menossa lomalle tänään.";
int indeksi = teksti.IndexOf("lomalle");
string uusiTeksti = teksti.Replace(teksti.Substring(indeksi, 7), "matkalle");
```

Tässä koodissa haetaan ensin "lomalle" -tekstin indeksi ja sen jälkeen Replace-metodilla korvataan tämä teksti "matkalle" -tekstillä.

## Syvällinen sukellus

Replace-metodi pystyy myös muotoilemaan tekstiä eri tavoin. Voit esimerkiksi käyttää sitä muuttamaan kirjaimien kokoa tai vaihtamaan isot ja pienet kirjaimet paikkaa. Seuraavassa on joitain esimerkkejä:

```
string teksti = "TÄMÄ on TEXTIä";
string pienetKirjaimet = teksti.ToLower();
string isotKirjaimet = teksti.ToUpper();
```

Tässä koodissa pienetKirjaimet-muuttujaan tallennetaan teksti pienin kirjaimin ja isotKirjaimet-muuttujaan tallennetaan teksti isoilla kirjaimilla.

Lisäksi Replace-metodi tukee myös regular expressioneita, joka avaa vielä enemmän mahdollisuuksia tekstien muokkaamiseen.

## Katso myös

- [C# Replace-metodi (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0)
- [C# IndexOf-metodi (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.indexof?view=net-5.0)
- [C# Regular expressionit (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)