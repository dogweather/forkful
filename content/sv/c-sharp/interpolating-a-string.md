---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Interpolering av strängar i C# innebär att vi kan injicera variabler direkt i en sträng, vilket gör vår kod mer läsbar och konsis. Utvecklare använder detta som ett alternativ till att använda strängkonkatenering eller formatmetoder.

## Så här gör du:
Här är en enkel kod som visar hur man interpolerar en sträng i C#.

```C#
string name = "Peter";
int age = 30;
string greeting = $"Hej {name}, du är {age} år gammal.";
Console.WriteLine(greeting); // Output: Hej Peter, du är 30 år gammal.
```

Följande kod visar hur man formaterar tal med stränginterpolation.

```C#
double price = 99.99;
string product = "bok";
string message = $"Priset för en {product} är {price:C}.";
Console.WriteLine(message); // Output: Priset för en bok är 99,99 kr.
```

## Djupdykning
Stränginterpolation introducerades först i C# 6.0, vilket var ett steg framåt från att använda `String.Format` eller konkatenering, vilket kan bli rörigt för komplexa strängar. Ett alternativ till detta är att använda `StringBuilder`, särskilt när du hanterar stora mängder data.

Stränginterpolation i C# fungerar genom att först översätta din interpolerade sträng till en `String.Format()`-kod. Det betyder att `$"Hej {name}, du är {age} år gammal."` faktiskt översätts till `"Hej {0}, du är {1} år gammal.", name, age` bakom kulisserna.

## Se också
För ytterligare läsning och relaterad information, se följande länkar:
- Microsofts dokumentation på stränginterpolation [här](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- En jämförelse mellan stränginterpolation och andra metoder [här](https://www.codingame.com/playgrounds/6179/string-interpolation-vs-string-format)
- Ytterligare information om hur `String.Format()` fungerar [här](https://docs.microsoft.com/en-us/dotnet/api/system.string.format)