---
title:                "Läsa en textfil"
html_title:           "PowerShell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Läsning av en textfil är när vi tar innehållet från en textfil och visar det på skärmen. Det är vanligt att programmerare behöver läsa textfiler för att få åtkomst till information som lagras i filen.

## Så här gör du:

I PowerShell kan vi läsa en textfil med hjälp av kommandot `Get-Content`. Här är ett exempel på hur du kan använda detta kommando:

```PowerShell
# Spara sökvägen till din textfil i en variabel
$filePath = "C:\Users\Username\Desktop\example.txt"

# Läs innehållet från textfilen och spara det i en annan variabel
$text = Get-Content $filePath

# Visa innehållet på skärmen
$text
```

När du kör koden ovan kommer innehållet från din textfil att skrivas ut på skärmen.

## Fördjupning:

Att läsa textfiler har varit en viktig del av programmering under en lång tid. Innan PowerShell användes, var det vanligt att använda kommandot `cat` i terminalen på Unix-baserade system för att läsa textfiler. Idag finns det också andra alternativ för att läsa textfiler, som till exempel att använda en textredigerare eller programmeringsspråk som Python.

Det finns också flera sätt att implementera läsning av textfiler i PowerShell, till exempel med hjälp av `Get-ChildItem` eller `.NET` klasser som `StreamReader`.

## Se även:

- [PowerShell documentation for Get-Content](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Management/Get-Content?view=powershell-8)
- [How to read and write to text files in PowerShell](https://www.petri.com/powershell-problem-solver-reading-and-writing-text-files)
- [Cat command in Unix-based systems](https://www.geeksforgeeks.org/cat-command-in-linux-with-examples/)