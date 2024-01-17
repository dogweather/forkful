---
title:                "Innlesing av kommandolinjealternativer"
html_title:           "PowerShell: Innlesing av kommandolinjealternativer"
simple_title:         "Innlesing av kommandolinjealternativer"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Hvis du har brukt en datamaskin før, har du sannsynligvis sett noen som skriver kommandoer i et vindu som kalles et "terminalvindu". De mest avanserte brukerene kan bruke disse kommandoene til å starte programmer med spesielle instruksjoner. En del av disse spesielle instruksjonene kalles "kommmandolinjeargumenter". Programmerere bruker kommandolinjeargumenter for å spesifisere bestemte oppgaver som skal utføres når programmet kjører.

## Hvordan:
For å lese kommandolinjeargumenter i PowerShell, følg disse trinnene:

  ```PowerShell
  $arguments = $args # Lagrer argumentene som et objekt
  $arguments[0] # Viser første argument i listen
  ```

Output: Viser det første argumentet som du skrev når du kalte på programmet.

## En dypere dykk:
Kommandolinjeargumenter har vært en del av programmering helt siden de første datamaskinene ble laget. Det å bruke kommandolinjeargumenter for å spesifisere bestemte oppgaver som skal utføres, er en måte å gjøre programmering mer effektiv på. Alternativet til å bruke kommandolinjeargumenter er å inkludere alle parametere direkte i koden, som kan bli rotete og vanskelig å vedlikeholde over tid. Når det kommer til implementering, bruker PowerShell en spesialkommando kalt $args for å lese kommmandolinjeargumenter og lagre dem som et objekt som kan brukes i koden.

## Se også:
Hvis du ønsker å lære mer om kommandolinjeargumenter i PowerShell, kan du lese følgende kilder:

- [Microsoft dokumentasjon om kommandolinjeargumenter i PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_command_line_args?view=powershell-7.1)
- [PowerShell kurs og opplæringsvideoer på Pluralsight](https://www.pluralsight.com/courses/powershell-scripting-getting-started)
- [StackOverflow spørsmål om kommandolinjeargumenter i PowerShell](https://stackoverflow.com/questions/34684103/how-to-pass-multiple-arguments-to-a-powershell-script-file)