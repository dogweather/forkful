---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Lage en midlertidig fil i PowerShell

## Hva & Hvorfor?

Å lage en midlertidig fil innebærer å opprette en fil som brukes midlertidig og vanligvis slettes etter bruk. Programmerere gjør dette for å lagre data midlertidig under en prosess og redusere minnebruken.

## Hvordan gjøre dette:

Å opprette en midlertidig fil i PowerShell er ikke så vanskelig som du kanskje tror. La oss kaste oss rett inn i eksempelet:

```PowerShell
$tempPath = [System.IO.Path]::GetTempFileName()
New-Item -ItemType file -Path $tempPath -Force

# Skriv noe til den midlertidige filen
Set-Content -Path $tempPath -Value "Hei, verden!"

# Les fra den midlertidige filen
Get-Content -Path $tempPath
```

Når du kjører koden ovenfor, oppretter du først en midlertidig fil. Så skriver du en verdi til filen og leser verdien tilbake.

## Dypdykk 

Tidligere, før PowerShell og moderne programmeringsspråk, ville programmerere skrive midlertidige filer direkte til disken ved å bruke lavnivås datahåndtering metoder. Dette kan fremdeles gjøres, men høyere nivåverktøy som PowerShell gir en enklere tilnærming.

Alternativt, hvis du er mer komfortabel med .Net, kan du bruke `System.IO.Path.GetTempFileName()` til å opprette en midlertidig fil.

Når det gjelder implementeringsdetaljer, oppretter `GetTempFileName()` faktisk en fil for deg i det midlertidige mappen. Funksjonen returnerer en streng som representerer filbanen til den nylig opprettede filen.

## Se også

For å dykke dypere inn i dette emnet, se disse kildene:

- Microsofts dokumentasjon om GetTempFileName-metoden: https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename
- StackOverflow-tråd om opprettelse av en midlertidig fil i PowerShell: https://stackoverflow.com/questions/30041174/powershell-create-temp-file-with-specific-extension