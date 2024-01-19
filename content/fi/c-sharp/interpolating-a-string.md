---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Merkkijonon interpolaatio C#-ohjelmointikielessä tarkoittaa muuttujien, ilmaisujen ja menetelmäkutsujen sisällyttämistä suoraan merkkijonojen sisään. Tätä tehdään koodin selkeyttämiseksi ja lukemisen helpottamiseksi.

## Miten:

Voit suorittaa merkkijonon interpolaation C#-kielessä käyttämällä `$`-merkkiä ennen merkkijonoa. Tämän jälkeen voit sisällyttää muuttujia ja ilmaisuksia `{}`-sulkujen sisälle.

```C#
int apples = 5;
int oranges = 6;
string result = $"Minulla on {apples} omenaa ja {oranges} appelsiinia.";
Console.WriteLine(result);
```

Tämä koodi tulostaa: "Minulla on 5 omenaa ja 6 appelsiinia."

## Syvällä:

Merkkijonon interpolaatio otettiin käyttöön C# 6.0 -version myötä vuonna 2015. Tätä ennen merkkijonoihin yhdistettiin usein muuttujia `string.Format` -menetelmän tai konkatenoinnin avulla.

```C#
// Ennen interpolaatiota:
string result = string.Format("Minulla on {0} omenaa ja {1} appelsiinia.", apples, oranges);

// tai

string result = "Minulla on " + apples + " omenaa ja " + oranges + " appelsiinia.";
```

Merkkijonon interpolaation avaintoteutus on `FormattableString`-luokka, joka kääntää interpolaatiomerkkijonot formatointimerkkijonoiksi ja argumenttilistoiksi.

## Katso myös:

- Microsoftin dokumentaatio merkkijonon interpolaatiosta C#-kielessä: [https://docs.microsoft.com/fi-fi/dotnet/csharp/language-reference/tokens/interpolated](https://docs.microsoft.com/fi-fi/dotnet/csharp/language-reference/tokens/interpolated)

- `FormattableString`-luokka: [https://docs.microsoft.com/fi-fi/dotnet/api/system.formattablestring](https://docs.microsoft.com/fi-fi/dotnet/api/system.formattablestring)