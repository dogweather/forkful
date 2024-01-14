---
title:                "C#: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne analysere og tolke HTML-kode kan være en nyttig ferdighet for programmerere, spesielt når man jobber med å hente data fra nettsider eller utvikle webapplikasjoner. Ved å utføre såkalt "parsing" av HTML-kode, kan man få tilgang til spesifikke data og elementer på en mer effektiv måte enn å manuelt lese koden.

## Hvordan

For å begynne å parse HTML-kode i C#, må du først importere klassene og metodene fra System.Net og System.IO namespaces. Deretter kan du bruke en WebClient-klasse til å laste ned HTML-koden fra en nettside og lagre den i en string. I følgende eksempel skal vi finne og lese innholdet av en <p> tag på en nettside:

```
using System;
using System.Net;
using System.IO;

WebClient client = new WebClient();
string htmlCode = client.DownloadString("https://www.example.com");
string tag = "<p>";
int startIndex = htmlCode.IndexOf(tag) + tag.Length;
int endIndex = htmlCode.IndexOf("</p>", startIndex);
string content = htmlCode.Substring(startIndex, endIndex - startIndex);
Console.WriteLine(content); // output: Velkommen til min blogg!
```

I dette eksempelet bruker vi metodene IndexOf() og Substring() for å finne og ekstrahere den ønskede teksten mellom <p> taggen. Dette kan også gjøres for andre HTML-elementer som <h1>, <a>, eller <div>. 

## Dykk dypere

Selv om dette eksempelet kun viser en enkel måte å hente ut data fra HTML-kode, finnes det flere avanserte metoder og verktøy for å utføre parsing. For eksempel kan du bruke HTMLAgilityPack-biblioteket som gir et mer fleksibelt og kraftig verktøy for å navigere og hente ut data fra HTML-kode. Det er også viktig å ta høyde for at ikke all HTML-kode vil være korrekt og konsistent, og du må derfor være forberedt på å håndtere forskjellige scenarier og feil.

## Se også
- [HTMLAgilityPack dokumentasjon](https://html-agility-pack.net/)
- [Parsing HTML with C# guide (Engelsk)](https://www.oreilly.com/library/view/adoactive-directory-o-auth/9781449320855/ch04s03.html)
- [C# string methoder for strings (Engelsk)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)