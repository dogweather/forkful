---
aliases:
- /no/vba/writing-a-text-file/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:24.685030-07:00
description: "\xC5 skrive en tekstfil i Visual Basic for Applications (VBA) involverer\
  \ \xE5 opprette, modifisere eller legge til tekstdata i filer, en grunnleggende\
  \ oppgave\u2026"
lastmod: 2024-02-18 23:08:53.747559
model: gpt-4-0125-preview
summary: "\xC5 skrive en tekstfil i Visual Basic for Applications (VBA) involverer\
  \ \xE5 opprette, modifisere eller legge til tekstdata i filer, en grunnleggende\
  \ oppgave\u2026"
title: Skrive en tekstfil
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive en tekstfil i Visual Basic for Applications (VBA) involverer å opprette, modifisere eller legge til tekstdata i filer, en grunnleggende oppgave for lagring av utdata, logging eller interaksjon med andre applikasjoner. Programmerere benytter denne funksjonaliteten for å automatisere rapportering, dataeksport eller generering av konfigurasjonsfiler innen Microsoft Office-økosystemet.

## Hvordan:

VBA tilbyr flere metoder for å skrive til en fil, men en av de mest rett frem metodene er å bruke `FileSystemObject`. Her er en steg-for-steg-guide for å opprette en enkel tekstfil og skrive data til den:

1. **Referer til Microsoft Scripting Runtime**: Først, sørg for at VBA-editoren din har tilgang til `FileSystemObject`. Gå til Verktøy > Referanser i VBA-editoren og merk av for "Microsoft Scripting Runtime."

2. **Opprett en tekstfil**: Følgende VBA-kodebit demonstrerer hvordan du oppretter en tekstfil og skriver en linje tekst inn i den.

```vb
Sub WriteToFile()
    Dim fso As FileSystemObject
    Set fso = New FileSystemObject
    
    Dim textFile As Object
    ' CreateTextFile parametere: (Filnavn, Overskriv, Unicode)
    Set textFile = fso.CreateTextFile("C:\yourPath\example.txt", True, False)
    
    ' Skriv en linje med tekst
    textFile.WriteLine "Hello, VBA!"
    
    ' Lukk filen
    textFile.Close
End Sub
```

Dette scriptet oppretter (eller overskriver hvis den allerede eksisterer) en fil med navnet `example.txt` i den spesifiserte mappen og skriver "Hello, VBA!" inn i den før filen lukkes for å lagre endringene.

3. **Eksempel på utdata**:

Etter å ha kjørt VBA-scriptet ovenfor, vil du finne en fil med navnet `example.txt` som inneholder følgende innhold:

```
Hello, VBA!
```

## Dypdykk:

`FileSystemObject` (FSO), en del av Microsoft Scripting Runtime-biblioteket, tilbyr et rikt sett med egenskaper og metoder for filoperasjoner, som går utover det tradisjonell VBA-filhåndtering tilbyr (f.eks., `Open`, `Print` #, `Write` #). I tillegg til håndtering av filer, kan FSO også manipulere mapper og disker, noe som gjør det til et kraftig verktøy for filsystemoperasjoner innen VBA.

Det er verdt å merke seg, imidlertid, at mens FSO presenterer en mer moderne tilnærming til filoperasjoner i VBA, kan det introdusere overhead for enkle oppgaver sammenlignet med VBA sin innebygde filhåndteringsinstrukser. Videre, siden FSO er en del av et eksternt bibliotek, kan det være bekymringer knyttet til bærbarhet og kompatibilitet med andre systemer (f.eks., eldre versjoner av Office, Mac Office).

I sammenhenger hvor ytelse, kompatibilitet eller minimal ekstern avhengighet er kritisk, kan programmerere vurdere å bruke VBA's innebygde filhåndteringsteknikker. Men for mer komplekse operasjoner eller når man arbeider innenfor et miljø hvor disse bekymringene er redusert (som i en kontrollert bedriftsinnstilling), oppveier ofte fordelene med FileSystemObject ulempene.
