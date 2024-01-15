---
title:                "Å lese en tekstfil"
html_title:           "C#: Å lese en tekstfil"
simple_title:         "Å lese en tekstfil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor lese en tekstfil i C#? Vel, det er et nyttig verktøy for å behandle og behandle data i et strukturert format. Enten du trenger å lese filer som inneholder brukerinformasjon, loggfiler, eller annen type data, vil kunnskap om hvordan du leser en tekstfil være nyttig.

## Hvordan

For å lese en tekstfil i C#, må du først åpne en filstrøm til filen. Dette kan gjøres med `FileStream` -klassen:

```C#
FileStream filStream = new FileStream("filnavn.txt", FileMode.Open);
```

Deretter kan du bruke `StreamReader` -klassen til å lese data i filen, linje for linje, ved hjelp av `ReadLine()` -metoden:

```C#
StreamReader leser = new StreamReader(filStream);
string linje = leser.ReadLine();
```

Nå kan du behandle dataen som du ønsker, enten det er å skrive den ut på konsollen, lagre den i en variabel, eller manipulere den på andre måter.

```C#
Console.WriteLine(linje); // skriver ut den leste linjen på konsollen
```

For å lukke filstrømmen og frigjøre ressurser, må du bruke `Close()` -metoden til leseren:

```C#
leser.Close();
```

Etter at filen er blitt lest, må du lukke filstrømmen ved hjelp av `Close()` -metoden til filstrømmen:

```C#
filStream.Close();
```

## Deep Dive

Når du leser en tekstfil, kan du bruke forskjellige metoder og funksjoner til å lese dataen. For eksempel kan du bruke `Read()` -metoden til `StreamReader` -klassen for å lese en bestemt mengde tegn i filen. Eller, hvis du forventer at filen kan inneholde feilaktige data, kan du bruke `Peek()` -metoden for å sjekke om den neste linjen i filen er gyldig før du prøver å lese den.

Det finnes også forskjellige måter å lese filen på, for eksempel å lese den en linje om gangen ved hjelp av `ReadLine()` -metoden, som vist i eksemplene over. Du kan også bruke `ReadToEnd()` -metoden for å lese hele filen på en gang.

Det er også viktig å huske på å håndtere eventuelle unntak som kan oppstå mens du leser en fil, for eksempel hvis filen ikke eksisterer eller ikke kan åpnes. Dette kan gjøres ved å bruke `try-catch` -blokker rundt kode som kan kaste et unntak.

## Se også

- Introduksjon til filstrømmer i C# (https://docs.microsoft.com/nb-no/dotnet/standard/io/file-streams)
- Dokumentasjon for `StreamReader`-klassen (https://docs.microsoft.com/nb-no/dotnet/api/system.io.streamreader)
- Fordeler og ulemper med å lese tekstfiler i C# (https://www.c-sharpcorner.com/blogs/reading-text-files-in-c-sharp1)