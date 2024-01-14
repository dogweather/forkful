---
title:                "C#: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standardfeil er en vanlig praksis for utviklere som ønsker å fange eventuelle feil eller unntak som oppstår under kjøringen av et program. Det lar deg enkelt spore og feilsøke problemer under utviklingsprosessen.

## Hvordan

Du kan skrive til standardfeil ved hjelp av Console klasse i C#. Her er en enkel kode som fanger en exception og skriver den til standardfeil:

```C#
try {
    // din kode
}
catch (Exception e) {
    Console.Error.WriteLine("Feil oppsto: " + e.Message);
}
```

Når programmet kjøres, vil output bli vist i rødt i konsollen og inneholde feilmeldingen fra unntaket. Dette gjør det enklere å identifisere problemet og fikse det.

## Dypdykk

Å skrive til standardfeil er spesielt nyttig når du vil logge informasjon om unntak som oppstår under programkjøringen. Med metoden `WriteLine` fra `Console.Error`-objektet kan du også skrive ut andre typer informasjon til standardfeil, for eksempel advarselsmeldinger eller statusoppdateringer.

Det er også verdt å nevne at standardfeil kan kobles til andre outputstrømmer, for eksempel filer eller nettverksporter, som gjør det mulig å lagre feilinformasjon på en mer permanent måte.

## Se også

- [C# Console Klasse](https://docs.microsoft.com/en-us/dotnet/api/system.console?view=netcore-3.1)
- [Unntakshåndtering i C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions/)
- [Feilsøking i C#](https://docs.microsoft.com/en-us/dotnet/games/introduction-to-debugging-in-csharp/)