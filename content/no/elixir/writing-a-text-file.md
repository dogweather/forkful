---
title:    "Elixir: Å skrive en tekstfil"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

**## Hvorfor**

Skriver du ofte tekstfiler som en del av din programmeringsoppgave? Hvis ja, så vil du få stor nytte av å lære å skrive tekstfiler i Elixir. I denne bloggposten vil jeg guide deg gjennom hvordan du kan gjøre akkurat det!

**## Hvordan**

For å skrive en tekstfil i Elixir, må du bruke `File.write/3`-funksjonen. La oss si at du ønsker å skrive en fil kalt "test.txt" med innholdet "Hei, verden!". Følgende eksempelkode viser hvordan du kan gjøre dette:

```Elixir
File.write("test.txt", "Hei, verden!")
```

Når du kjører koden, vil du se at en fil med navnet "test.txt" har blitt opprettet i samme mappe som filen du kjørte koden fra. Hvis du åpner filen, vil du se at den inneholder teksten "Hei, verden!".

Du kan også legge til en valgfri tredje parameter i `File.write`-funksjonen som angir om filen skal erstattes hvis den allerede eksisterer. Sett denne parameteren til `:append` hvis du vil legge til tekst i slutten av en allerede eksisterende fil. Her er et eksempel:

```Elixir
File.write("test.txt", " verden!", :append)
```

Dette vil legge til " verden!" i slutten av "Hei," i filen "test.txt".

**## Dypdykk**

Nå som du vet hvordan du skriver en tekstfil i Elixir, la oss se på noen flere detaljer om denne funksjonen.

For det første, hvis du prøver å skrive til en fil som er åpen i et annet program, vil du få en feilmelding. Elixir vil ikke la deg overskrive en fil som allerede er i bruk.

En annen viktig ting å merke seg er at `File.write` returnerer en `:ok`-atom hvis skrivingen var vellykket, og en `{:error, reason}`-tuppel hvis noe gikk galt. Dette kan være nyttig å inkludere i logikken din for å håndtere eventuelle feil som kan oppstå.

Til slutt kan du også skrive flerlinjede tekster ved å bruke `"""`-notationen eller ved å bruke `\n` for å lage linjeskift. Her er et eksempel på begge:

```Elixir
File.write("test.txt", """Hei,
verden!""")
```

```Elixir
File.write("test.txt", "Hei,\nverden!")
```

**## Se også**

- [Elixir Dokumentasjon: File](https://hexdocs.pm/elixir/File.html)
- [Elixir Kommunitet: Writing Text Files](https://elixirforum.com/t/writing-a-text-file-with-elixir/7447)