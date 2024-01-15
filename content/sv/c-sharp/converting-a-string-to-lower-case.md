---
title:                "Omvandla en sträng till små bokstäver"
html_title:           "C#: Omvandla en sträng till små bokstäver"
simple_title:         "Omvandla en sträng till små bokstäver"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener är en vanlig operation inom programmering, särskilt när det kommer till hantering av olika användarinput. Genom att konvertera en sträng till gemener säkerställer vi att all text behandlas på samma sätt och minimerar därmed risken för felaktig jämförelse eller sökning.

## Så här gör du

```C#
string input = "HeLLo WoRLd!";
string output = input.ToLower();
Console.WriteLine(output);
```

Kodexemplet ovan visar hur man enkelt kan konvertera en sträng till gemener med hjälp av den inbyggda metoden "ToLower()" i C#. Resultatet blir "hello world!".

Det är även möjligt att använda en annan metod, "ToLowerInvariant()", som konverterar till gemener baserat på det aktuella kulturtillståndet. Om man till exempel har en app som ska fungera på flera språk kan det vara fördelaktigt att använda denna metod för att säkerställa korrekt konvertering oavsett vilket språk användaren har inställt på sin enhet.

```C#
string input = "HeLLo WoRLd!";
string output = input.ToLowerInvariant();
Console.WriteLine(output);
```

## Djupdykning

Att konvertera en sträng till gemener är en process som kan verka enkel och självklar, men det finns några viktiga saker att tänka på för att undvika oväntade resultat. En viktig faktor är språket som användaren har inställt på sin enhet eller i sin app. Vissa språk, som till exempel tyska, har egna regler för konvertering av bokstäver från versal till gemen. Det kan vara värt att undersöka dessa regler för att undvika eventuella konverteringsfel.

En annan aspekt att tänka på är eventuella specialtecken eller diakritiska tecken i strängen som ska konverteras. Vissa tecken kan försvinna eller ändras i konverteringsprocessen och det är viktigt att VARNA användaren för detta, om det är relevant, för att undvika missförstånd.

## Se även

* Microsoft Docs - ToLower()-metoden: https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower
* Microsoft Docs - ToLowerInvariant()-metoden: https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant
* Språkets påverkan på konvertering av gemener: https://stackoverflow.com/questions/3521085/string-tolower-some-languages-change-things-you-wouldnt-expect