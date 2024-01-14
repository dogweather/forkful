---
title:                "C#: Oppretting av midlertidig fil"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Hvorfor

 Hvorfor opprette en midlertidig fil?

Noen ganger i programmeringen kan det være nyttig å opprette midlertidige filer for å lagre informasjon underveis i en prosess. Dette kan for eksempel være i tilfeller der man trenger å lagre data som ikke er nødvendig å beholde etter at programmet er avsluttet.

# Hvordan

For å opprette en midlertidig fil i C#, kan man bruke "Path.GetTempFileName" metoden. Denne metoden vil automatisk generere en unik filnavn som kan brukes til å opprette en midlertidig fil. Her er et eksempel på hvordan man kan bruke denne metoden:

```C#
string tempFileName = Path.GetTempFileName();
Console.WriteLine(tempFileName);
```

Eksempel på output: C: \Users\Brukernavn\AppData\Local\Temp\tmp9876.tmp

# Deep Dive

Man kan også spesifisere et alternativt navn på den midlertidige filen ved å gi en parameter til "Path.GetTempFileName" metoden. Dette kan være nyttig hvis man ønsker et mer meningsfylt navn på filen istedenfor et generert navn.

I tillegg kan man bruke "File.Create" metoden for å opprette den midlertidige filen og deretter jobbe med den som en vanlig fil. Det er viktig å huske på å slette den midlertidige filen etter at man er ferdig med å bruke den, for å ikke fylle opp maskinen med unødvendige filer.

# Se også

Her er noen nyttige ressurser for å lære mer om oppretting og håndtering av midlertidige filer i C#:

- Dokumentasjon for Path.GetTempFileName metoden: https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=netframework-4.8
- Tutorial om midlertidige filer på C# Tutorials-nettstedet: https://csharp.net-tutorials.com/files/temporary-files/
- Forumtråd om oppretting og sletting av midlertidige filer i C#: https://stackoverflow.com/questions/986708/creating-and-deleting-temporary-files-in-c-sharp