---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "C#: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon pieniksi kirjaimiksi? On monia syitä, mutta yleisin syy on yhtenäisten tietojen käsittely ja vertailu. 

## Miten

Merkkijonon muuttaminen pieniksi kirjaimiksi C# -ohjelmointikielessä on helppoa ja nopeaa. Käytä vain ```ToLower()``` -metodia ja anna haluamasi merkkijono sulkuihin.

```C#
string esimerkki = "TÄMÄ ON ESIMERKKI";
string pienet = esimerkki.ToLower();
Console.WriteLine(pienet);
```

Tulostus:
```C#
tämä on esimerkki
```

Voit myös suoraan muuttaa merkkijonon pieniksi kirjaimiksi ilman väliaikaista muuttujaa, kuten alla olevassa esimerkissä.

```C#
Console.WriteLine("TÄMÄ ON ESIMERKKI".ToLower());
```

Tulostus:
```C#
tämä on esimerkki
```

## Syvällinen sukellus

C# -ohjelmointikielellä on monia erilaisia metodeja ja funktioita merkkijonojen käsittelyyn, mutta ```ToLower()``` -funktio on yksi yksinkertaisimmista ja kätevimmistä. Se muuntaa kaikki merkit merkkijonossa pieniksi kirjaimiksi ja palauttaa uuden merkkijonon.

On myös hyvä huomata, että ```ToLower()``` toimii vain ASCII-merkkijonoille. Jos haluat muuntaa esimerkiksi skandinaavisia merkkejä pieniksi kirjaimiksi, kannattaa käyttää ```ToLowerInvariant()``` -funktiota.

```C#
Console.WriteLine("ÄITI".ToLower());
```
Tulostus:
```C#
ÄITI
```
```C#
Console.WriteLine("ÄITI".ToLowerInvariant());
```

Tulostus:
```C#
äiti
```

## Katso myös

- [Microsoftin virallinen dokumentaatio "ToLower()" -funktiosta](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
- [W3Schoolsin selkeä esimerkki merkkijonon muuttamisesta pieniksi kirjaimiksi C# -ohjelmointikielessä](https://www.w3schools.com/cs/cs_case.asp)
- [Tarkempi opas merkkijonon muuttamiseen pieniksi kirjaimiksi ja moniin muihin käsittelytapoihin C# -ohjelmointikielessä](https://www.dotnetperls.com/tolower)