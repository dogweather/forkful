---
title:                "Ekstrahering av understrenger"
html_title:           "C#: Ekstrahering av understrenger"
simple_title:         "Ekstrahering av understrenger"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville man ønske å utvinne substrings i C#? En av grunnene kan være for å manipulere tekststrenger på en mer presis og effektiv måte.

## Hvordan

Det er flere måter å utvinne substrings i C# på, avhengig av hva du ønsker å oppnå.

### Utvinne en enkelt del av en tekststreng 

Hvis du for eksempel ønsker å utvinne "world" fra "Hello world!", kan du bruke metoden `Substring` på følgende måte:

```C#
string text = "Hello world!";
string substring = text.Substring(6, 5); // starter på index 6 og henter 5 tegn
Console.WriteLine(substring); // output: world
```

### Utvinne flere deler av en tekststreng

Hvis du ønsker å utvinne flere deler av en tekststreng, som for eksempel "Hello" og "!". kan du bruke metoden `Split` på følgende måte:

```C#
string text = "Hello world!";
string[] substrings = text.Split(" "); // separerer på mellomrom
Console.WriteLine(substrings[0]); // output: Hello
Console.WriteLine(substrings[2]); // output: !
```

### Endre tekststreng basert på en substring

Du kan også enkelt bytte ut en del av en tekststreng ved å bruke metoden `Replace`:

```C#
string text = "Hello world!";
text = text.Replace("world", "everyone"); // bytter ut "world" med "everyone"
Console.WriteLine(text); // output: Hello everyone!
```

## Dypdykk

Det finnes flere ulike metoder og teknikker for å utvinne substrings i C#. Hvis du ønsker å lære mer om detaljene og hvordan du kan bruke dem til å løse mer avanserte oppgaver, kan du utforske følgende ressurser:

- [Microsoft sin dokumentasjon om `Substring` og `Split` metoden](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- [W3Schools sin gjennomgang av utvinning av substrings i C#](https://www.w3schools.com/cs/cs_strings_substrings.asp)
- [En guide til manipulering av tekst i C# av TutorialsTeacher](https://www.tutorialsteacher.com/csharp/csharp-string)
- [Et nettsted med ulike eksempler og øvelser for å lære om substrings i C#](https://www.interviewbit.com/csharp-tutorial/string-manipulation/)

## Se også

Her er noen relevante artikler og ressurser for å lære mer om tekstmanipulering i C#:

- [C# String Manipulation Tutorial av TechBeamers](https://www.techbeamers.com/csharp-string-tutorial/)
- [10 Useful C# String Functions You Should Know av The Crazy Programmer](https://www.thecrazyprogrammer.com/2019/02/c-sharp-string-functions.html)
- [An introduction to working with strings in C# av FreeCodeCamp](https://www.freecodecamp.org/news/csharp-string-tutorial-how-to-work-with-strings-in-csharp/)