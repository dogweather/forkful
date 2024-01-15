---
title:                "Mallin perustuvien merkkien poistaminen"
html_title:           "C#: Mallin perustuvien merkkien poistaminen"
simple_title:         "Mallin perustuvien merkkien poistaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi voisi haluta poistaa merkkejä, jotka vastaavat tiettyä kaavaa ohjelmointityössä. Yleisimpiä syitä ovat tiedon käsittely ja datan puhdistaminen ennen sen käyttöä.

## Näin Tehdään

Merkkijonon tiettyjen merkkien poistaminen voidaan tehdä monella eri tavalla C#-ohjelmoinnissa. Yksi tapa on käyttää Replace-metodia, joka korvaa halutut merkit tyhjällä merkillä. Toinen tapa on käyttää Regex-luokkaa, joka mahdollistaa monimutkaisempien kaavojen käytön.

```C#
// Replace-metodin käyttö:
string originalString = "Hello123World";
string replacedString = originalString.Replace("123", "");
Console.WriteLine(replacedString); // Tulostaa "HelloWorld"

// Regex-luokan käyttö:
string originalString = "Hello123World";
string pattern = @"\d+"; // Merkkijono, jota halutaan poistaa
string replacedString = Regex.Replace(originalString, pattern, "");
Console.WriteLine(replacedString); // Tulostaa "HelloWorld"
```

## Syvemmälle

Regex-luokka mahdollistaa monipuolisen tavan poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Esimerkiksi, voit käyttää kaavassa sanaa "ja" osoittamaan, että merkkijonossa pitää olla sekä "a" että "j". Voit myös käyttää erilaisia metakaraktereja, kuten "[]" tai "()". Regex-luokkaa käyttäessä on hyvä olla perillä eri metakarakterien merkityksistä ja käyttötavoista.

## Katso myös

- [C# Replace-metodin dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netcore-3.1)
- [C# Regex-luokan dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1)
- [Regex-metakarakterit ja niiden käyttö C#-ohjelmoinnissa](https://www.regular-expressions.info/quickstart.html)