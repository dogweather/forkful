---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera en sträng till små bokstäver innebär att ändra varje stor bokstav till motsvarande liten bokstav i en given sträng. Programmerare gör detta för att standardisera data, vilket underlättar jämförelser och sökningar.

## Hur man gör:

Här är ett enkelt exempel på hur man konverterar en sträng till små bokstäver med PowerShell:

```PowerShell
# Definiera en sträng med stora bokstäver
$originalString = "HEJ VÄRLDEN"

# Konvertera strängen till små bokstäver
$lowercaseString = $originalString.ToLower()

# Skriv ut resultatet
Write-Output $lowercaseString
```

När du kör koden ovan kommer utdata att vara "hej världen".

## Djupdykning

Konvertera strängar till små bokstäver är inte en ny idé - faktum är att det har varit en standardverktyg för programmerare sedan de tidigaste dagarna av programmering. För dem som kommer från andra programmeringsspråk, till exempel Python, kanske du känner igen `lower()`-metoden, som gör samma sak.

Som alternativ, använd `.ToLowerInvariant()` i stället för `.ToLower()` när det gäller att hantera data för internationell programmering. Detta beror på att `.ToLowerInvariant()` skapar en kulturneutral representation av strängen, vilket kan vara fördelaktigt för att undvika oväntade problem.

Från en implementeringssynpunkt använder ToLower()-metoden i .NET Framework informationsborddefinierade av Unicode-standarden för att mappa varje stor bokstav till motsvarande gemene bokstav. 

## Se också

Här är några användbara referenser för att utforska mer om strängmanipulering i PowerShell:

1. Officiell dokumentation för [PowerShell](https://docs.microsoft.com/en-us/powershell/).
3. [.NET String.ToLower Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower).