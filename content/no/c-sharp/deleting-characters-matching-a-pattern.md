---
title:                "C#: Slette tegn som matcher et mønster"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Hvorfor

Noen ganger kan det være nødvendig å fjerne visse karakterer fra en streng som følger et bestemt mønster. Dette kan være nyttig for å lage et filter for sensitiv informasjon, eller for å forenkle dataene vi jobber med. I denne artikkelen skal vi se på hvordan vi kan slette karakterer som matcher et mønster i C#.

# Hvordan

For å fjerne karakterer som matcher et mønster i C#, kan vi bruke metoden `Regex.Replace()`. Denne metoden tillater oss å erstatte en eller flere strenger som følger et bestemt mønster med en tom streng, som i praksis vil fjerne disse karakterene.

La oss se på et eksempel der vi ønsker å fjerne alle tallene fra en streng:

```C#
var input = "Denne strengen inneholder tall: 12345";
var pattern = @"\d+"; // Dette er mønsteret for tall
var output = Regex.Replace(input, pattern, ""); // output vil nå være "Denne strengen inneholder tall:"
```

Her ser vi at vi har brukt `\d` for å representere tall, og `+` for å indikere at vi ønsker å fjerne alle tall som består av flere enn én siffer. Dette er et enkelt eksempel, men `Regex.Replace()` metoden har mange muligheter for å spesifisere mer komplekse mønstre.

Vi kan også bruke denne metoden til å fjerne bokstaver eller andre symboler. For eksempel kan vi fjerne alle vokaler fra en streng ved å bruke mønsteret `[aeiou]`:

```C#
var input = "Dette er en streng med mange vokaler";
var pattern = "[aeiou]"; // Mønsteret for vokaler
var output = Regex.Replace(input, pattern, ""); // output vil nå være "Dtt r n strng mng vklr"
```

# Dypdykk

Når vi bruker `Regex.Replace()` metoden, er det viktig å være oppmerksom på at den tar imot et regulært uttrykk som et argument. Regulære uttrykk er en spesialisering innen string matching, som tillater oss å søke etter visse mønstre eller sekvenser i en streng.

Det finnes flere ulike metoder og symboler for å lage regulære uttrykk, og dette går utover omfanget av denne artikkelen. Men det er verdt å nevne at det kan være lurt å øve seg på å lage og lese regulære uttrykk for å bli mer effektiv i å bruke `Regex.Replace()` (og andre metoder relatert til string matching).

# Se også

* [Microsoft sin dokumentasjon om `Regex.Replace()`](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace)
* [En guide til regulære uttrykk i C#](https://www.c-sharpcorner.com/article/the-basics-of-regular-expression-part-1/)