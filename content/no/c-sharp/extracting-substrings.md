---
title:                "Utvinning av delstrenger"
html_title:           "C#: Utvinning av delstrenger"
simple_title:         "Utvinning av delstrenger"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Noen ganger vil du som programmerer trenge å hente ut deler av en tekststreng, i stedet for å bruke hele strengen. Dette kalles å utvinne (extract) substrings, og det kan være nyttig for å lage mer fleksible og dynamiske kode.

## Hvordan:

Her er to måter å utvinne substrings på i C#:

```C#
string sentence = "Hei, jeg er en setning";
// Eksempel 1: Hent ut "jeg er"
string substring1 = sentence.Substring(5, 7);
Console.WriteLine(substring1); // utskrift: jeg er
// Eksempel 2: Hent ut "setning"
string substring2 = sentence.Substring(sentence.Length - 7);
Console.WriteLine(substring2); // utskrift: setning
```

## Dykk dypere:

Metoden "Substring" har eksistert i C# siden den første versjonen ble lansert i 2002. Men i nyere versjoner, som C# 8.0, har det blitt introdusert en mer moderne og intuitiv måte å utvinne substrings på ved hjelp av "Range" og operatorer. Dette gjør koden mer lesbar og lettere å forstå. En alternativ metode for å utvinne substrings er bruken av Regular Expressions, som lar deg søke og filtrere tekst på en mer avansert og fleksibel måte.

## Se også:

[Microsoft sin dokumentasjon om substrings i C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=netcore-3.1)

[Eksempler på bruk av Range og operatorer for å utvinne substrings](https://docs.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-8#indices)

[En introduksjon til Regular Expressions i C#](https://www.codeproject.com/Articles/9099/The-30-Minute-Regex-Tutorial)