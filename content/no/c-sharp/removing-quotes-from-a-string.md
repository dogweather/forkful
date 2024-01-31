---
title:                "Fjerne anførselstegn fra en streng"
date:                  2024-01-26T03:38:27.376379-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fjerne anførselstegn fra en streng"

category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å fjerne anførselstegn fra en streng i C# betyr at du tar bort de plagsomme doble (`"`) eller enkle (`'`) anførselstegnene som omslutter teksten din. Programmerere gjør dette for å rense data, forberede for databaseinnføring, eller gjøre strenger trygge for videre behandling så ting ikke går over styr når et tilfeldig anførselstegn dukker opp.

## Hvordan:
```csharp
string withQuotes = "\"Hei, verden!\"";
Console.WriteLine($"Original: {withQuotes}");

// Fjerner doble anførselstegn
string withoutDoubleQuotes = withQuotes.Replace("\"", "");
Console.WriteLine($"Uten doble anførselstegn: {withoutDoubleQuotes}");

// Fjerner enkle anførselstegn (under forutsetning av at strengen din hadde dem fra starten)
string withSingleQuotes = "'Hei, verden!'";
string withoutSingleQuotes = withSingleQuotes.Replace("'", "");
Console.WriteLine($"Uten enkle anførselstegn: {withoutSingleQuotes}");
```

Utdata:
```
Original: "Hei, verden!"
Uten Doble Anførselstegn: Hei, verden!
Uten Enkle Anførselstegn: Hei, verden!
```

## Dypdykk
Konseptet med å fjerne anførselstegn er ikke nytt eller spesielt komplekst, men det er kritisk fordi anførselstegn ofte brukes til å avgrense strenger. Når en streng med uescapede anførselstegn er inkludert i en kodeblokk eller en datafil, kan det avslutte strengen for tidlig, noe som forårsaker feil eller sikkerhetsproblemer som injeksjonsangrep.

Historisk sett har håndtering av anførselstegn vært en del av validerings- og saniteringsprosessen i databehandling. Mens `.Replace()`-metoden er enkel for å trekke anførselstegn ut av en enkel streng, kan du trenge mer avanserte teknikker som regulære uttrykk for å håndtere mer komplekse scenarioer, som nestede anførselstegn eller betinget fjerning.

Alternativer til `.Replace()` inkluderer metoder fra `Regex`-klassen når du trenger finjustert kontroll eller har å gjøre med mønstre i stedet for faste tegn. For eksempel kan `Regex.Unescape()` være praktisk når du håndterer escapede tegn.

Når det gjelder implementering, husk at strenger i C# er uforanderlige, noe som betyr at hver gang du bruker `.Replace()`, opprettes en ny streng. Dette er ikke et stort problem for små eller engangsoperasjoner, men det er noe å huske på ytelsesmessig for store eller mange strenger.

## Se også:
- [String.Replace Metode Dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netframework-4.8)
- [Regulære Uttrykk i .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [Beste Praksiser for Sikker Håndtering av Strenger](https://www.owasp.org/index.php/Data_Validation)
