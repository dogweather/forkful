---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Haskell: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Säännölliset lausekkeet tai tuttavallisemmin regex, ovat merkkijonoja, jotka kuvaavat merkkijonojen kuvioita. Ohjelmoijat käyttävät niitä arjen tehtäviin, kuten syötteen validointiin, merkkijonojen hakuun ja korvaamiseen sekä tekstin jakamiseen osiin.

## Näin tehdään:

Katsotaanpa kuinka tehdään muutamia perustyötehtäviä regexien avulla C#:ssa.
```C#
using System.Text.RegularExpressions;

// Sähköpostin validointi
string email = "testi@testi.fi";
bool IsValidEmail = Regex.IsMatch(email, 
@"^([a-zA-Z0-9_\-\.]+)@((\[[0-9]{1,3}" + 
@"\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\" + 
@".)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\]?)$");
Console.WriteLine(IsValidEmail); // Tulostaa: True

// Merkkijonojen etsintä
string teksti = "Hei, olen C# ohjelmoija";
Match match = Regex.Match(teksti, @"\bC#\b",
RegexOptions.IgnoreCase);
if (match.Success)
{
   Console.WriteLine("Löydetty: " + match.Value); // Tulostaa: Löydetty: C#
}
```

## Sukellus syvemmälle

Regex syntaksi perustuu UNIXin egrep-komentoon, ja se on peräisin 1950-luvulta. Vaihtoehtoisia tapoja merkkijonojen käsittelyyn ovat esimerkiksi perus merkkijono-funktiot, jotka voivat olla helpompia yksinkertaisissa tapauksissa. Toisaalta regex sallii monimutkaisemmat merkkijono-kuvion tarkistukset.

On tärkeää huomata, että Regex-luokka C#:ssa varastoi viimeksi käytetyn lausekkeen automaattisesti. Tilanteissa, joissa lauseke muuttuu harvoin, tämä optimoi suorituskykyä.

## Katso myös:

Dokumentaatio, jonka Microsoft tarjoaa, on erinomainen resurssi: [System.Text.RegularExpressions](https://docs.microsoft.com/fi-fi/dotnet/api/system.text.regularexpressions.regex)

Selkeä opas regexin perusteista löytyy myös osoitteesta: [regexone.com](https://regexone.com/)

Regexin testaamiseen on useita online työkaluja, kuten [regex101.com](https://regex101.com/)