---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "C#: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Säännölliset lausekkeet ovat tapa (1) löytää ja (2) työstää tietoja merkkijonoista tietyllä säännöllisellä kaavalla. Koodareille tämä tarkoittaa esimerkiksi tietojen validointia tai etsimistä tietokannasta tietyillä kriteereillä.

## Miten:
Käytä ```C#Regex``` luokkaa ja sen metodeja, kuten ```Match()``` ja ```Replace()```, säännöllisten lausekkeiden käyttämiseksi. Alla näet esimerkin, jossa tarkistamme onko annettu merkkijono validi sähköpostiosoite ja vaihdamme sen @-merkin tilalle %-merkin.

```C#
string email = "example%email.com";
string pattern = @"^\w+@[a-zA-Z_]+?\.[a-zA-Z]{2,3}$";
string replacement = "@";
string result = Regex.Replace(email, pattern, replacement);
// Output: "example@email.com" 
```

## Syventävää:
Säännölliset lausekkeet ovat olleet läsnä jo varhain tekstipohjaisessa tiedonkäsittelyssä, mutta vasta tietokoneiden yleistymisen myötä niiden käyttö on yleistynyt myös koodareiden keskuudessa. Nykyään on myös muita tapoja käsitellä säännöllisiä lausekkeita, kuten ilmaista kieltä käyttämällä. Tästäkin voi löytyä etuja esimerkiksi monikielisissä sovelluksissa. C# tarjoaa erilaisia metodeja ja ominaisuuksia säännöllisten lausekkeiden hallitsemiseksi, joten kannattaa tutustua niihin tarkemmin esimerkiksi Microsoftin dokumentaatiosta.

## Katso myös:
- [Microsoftin dokumentaatio säännöllisistä lausekkeista](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [C#Regex luokka](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=net-5.0)