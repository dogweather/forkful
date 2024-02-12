---
title:                "Lese en tekstfil"
aliases:
- /no/powershell/reading-a-text-file.md
date:                  2024-01-20T17:54:51.774665-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese en tekstfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese en tekstfil betyr å få tilgang til innholdet i filen via et program. Programmerere gjør dette for å behandle data, konfigurere programmer, eller automatisere oppgaver basert på tekstinnhold.

## Slik gjør du det:
Å lese fra en fil i PowerShell er enkelt. Her er et grunnleggende eksempel:

```PowerShell
$innhold = Get-Content -Path "sti/til/din/fil.txt"
$innhold
```

Resultatet vil være hver linje i filen skrevet ut til konsollen.

For å lese en fil linje for linje:

```PowerShell
$linjer = Get-Content -Path "sti/til/din/fil.txt"
foreach ($linje in $linjer) {
    $linje
}
```

Dette vil skrive ut hver linje individuelt.

## Dypdykk
Lesing av tekstfiler er grunnleggende i programmering. Det stammer fra tidlig i databehandlingens historie. I PowerShell er `Get-Content` kommandoen utbredt for denne oppgaven. Alternativer inkluderer bruk av .NET-klasser som `System.IO.StreamReader` for større kontroll eller ytelse:

```PowerShell
$streamReader = [System.IO.StreamReader] "sti/til/din/fil.txt"
while ($null -ne ($linje = $streamReader.ReadLine())) {
    $linje
}
$streamReader.Close()
```

Implementasjonsdetaljer som en bør tenke på inkluderer håndtering av store filer, tegnkoding (f.eks. UTF-8), og feilhåndtering. PowerShell kan arbeide effektivt med alle disse aspektene med litt ekstra konfigurasjon.

## Se Også
- Microsofts dokumentasjon om `Get-Content`: [docs.microsoft.com](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
- Om `System.IO.StreamReader` i .NET: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)
- PowerShell Gallery for delte skript og moduler: [powershellgallery.com](https://www.powershellgallery.com/)
