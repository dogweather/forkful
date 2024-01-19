---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Interpolasjon av tekststrenger i C# er prosessen for innsetting av variable verdier direkte til en tekststreng. Det gir programmerere en renere og mer lesbar kode ved formatering av strenger.

## Hvordan:

Vi kan interpolere en streng i C# ved hjelp av dollartegn ($). Se eksemplet nedenfor:

```C#
string navn = "Ola";
string hilsen = $"Hei, {navn}!";
Console.WriteLine(hilsen);
```

I utdata får vi følgende:

```
Hei, Ola!
```

Hvis vi vil formatere tallverdier, gjør vi det slik:

```C#
double pris = 1250.5;
string formatertPris = $"Prisen er {pris:C}.";
Console.WriteLine(formatertPris);
```

Utdataen blir da:

```
Prisen er kr 1 250,50.
```

## Dypdykk:

Historisk sett, før introduksjonen av strenginterpolasjon i C# 6.0, benyttet vi `String.Format`-metoden for å formatere tekststrenger. 

Det finnes andre alternativer til strenginterpolasjon, for eksempel `StringBuilder.Append` og konkatenasjon ved hjelp av pluss (+) operatøren. Men strenginterpolasjon gir mer lesbar og elegant kode.

Når det gjelder utførelsen, lager kompilatoren egentlig en formatert streng ved å bruke `String.Format` under panseret når vi bruker strenginterpolasjon. Dette gjør det like effektivt som å bruke `String.Format` direkte.

## Se Også:

- Microsoft Docs om strenginterpolasjon i C#: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated
- Stack Overflow diskusjon om effektiviteten av strenginterpolasjon: https://stackoverflow.com/questions/33044848/is-string-interpolation-in-c-6.0-efficient
- C# station tutorial om strengmanipulasjon i C#: https://csharp-station.com/HowTo/Strings