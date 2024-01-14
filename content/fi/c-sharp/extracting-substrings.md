---
title:    "C#: Palojen poimiminen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi

Substringien erottaminen on olennainen osa ohjelmointia ja sitä käytetään monissa eri koodauksen tilanteissa. Se voi auttaa sinua saamaan tietoja merkkijonoista, kuten sanoista tai lauseista, ja käsittelemään niitä haluamallasi tavalla. Tämä blogikirjoitus tulee opettamaan sinulle, kuinka voit helposti ja tehokkaasti erotella substringit C#-koodissasi.

## Kuinka

Erottaminen substringeiksi on helppoa C#-koodilla. Se voidaan tehdä usealla eri tavalla, mutta tässä keskitymme kolmeen esimerkkiin. Käytämme esimerkkikoodissa merkkijonoa "Tämä on esimerkki merkkijonosta".

```C#
string merkkijono = "Tämä on esimerkki merkkijonosta";
```

### Esimerkki 1: Substring(int startIndex, int length)

Tämä esimerkki käyttää Substring(int startIndex, int length) -metodia erottaakseen substringin alkamasta tietystä indeksistä ja halutun pituisena. Käyttämällä aiempaa merkkijonoamme, voimme erotella sanan "esimerkki" seuraavasti:

```C#
string sana = merkkijono.Substring(8, 8);
Console.WriteLine(sana);

// Tulostaa "esimerkki"
```

### Esimerkki 2: Split(char[] separator)

Toinen tapa erotella substringit on käyttää Split(char[] separator) -metodia, joka jakaa merkkijonon halutun erotusmerkin perusteella ja palauttaa taulukon. Jos siis haluamme erotella merkkijonossa olevat sanat erillisiin osiin, käytämme välilyöntiä "-merkkinä ja saamme seuraavanlaisen tuloksen:

```C#
string[] sanat = merkkijono.Split(' ');
Console.WriteLine(sanat[3]);

// Tulostaa "merkkijonosta"
```

### Esimerkki 3: Regex.Match(string input, string pattern)

Viimeinen esimerkki käyttää Regular Expression (Regex) -kirjastoa erottaakseen substringit halutun mallin perusteella. Käyttämällä aiempaa merkkijonoamme ja haluamme erottaa kaikki sanat, jotka alkavat kirjaimella "e", käytämme seuraavaa koodia:

```C#
Regex.Match(merkkijono, @"e\w+");

// Palauttaa Match-olion, jossa on kaikki e-kirjaimella alkavat sanat
```

## Syvemmälle

Yllä esittelyt esimerkit ovat vain pintaraapaisu substringien erottamisesta C#-koodissa. On olemassa monia muita tapoja, kuten Substring(int startIndex), jolla pystytään erottamaan substringin halutusta indeksistä loppuun asti, tai Regex.Split(string input, string pattern), jolla voidaan jakaa merkkijono monen eri mallin perusteella.

## Katso myös

- [C#-kielen virallinen dokumentaatio](https://docs.microsoft.com/fi-fi/dotnet/csharp/)
- [C# Regex-kirjaston dokumentaatio](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [C# Substring-metodin dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)