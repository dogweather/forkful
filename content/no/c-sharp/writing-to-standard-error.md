---
title:                "Skrive til standardfeil"
html_title:           "C#: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Skrevet til feilutgang er når du sender en melding eller en feilkode til standard feilutgangskanalen i stedet for standard utgangskanalen. Dette gjøres vanligvis når du ønsker å varsle om en uvanlig tilstand eller en feil i programmet ditt.

## Hvordan:
```
Console.Error.WriteLine("En uventet feil har oppstått!");
```
Output:
```
En uventet feil har oppstått!
```

```
int num1 = 10;
int num2 = 0;

try
{
    int result = num1 / num2;
}
catch (DivideByZeroException ex)
{
    Console.Error.WriteLine("Kan ikke dele på null!");
}
```
Output:
```
Kan ikke dele på null!
```

## Dypdykk:
Skrevet til feilutgang har vært en viktig del av programmering siden starten. Det gir en enkel måte å kommunisere feil og unike tilfeller til brukeren, uten å forstyrre standard utgangsmeldingene. Alternativet til å skrive til feilutgang er å bruke debuggervinduet, men dette kan være forstyrrende og mindre fleksibelt. Implementering av skriving til feilutgang i C# er enkelt, og er en metode som finnes i .NET Framework-klassen.

## Se også:
- https://docs.microsoft.com/en-us/dotnet/standard/io/errors-and-exceptions
- https://www.codegrepper.com/code-examples/csharp/write+to+standard+error
- https://www.c-sharpcorner.com/article/working-with-standard-input-output-and-error-in-C-Sharp/