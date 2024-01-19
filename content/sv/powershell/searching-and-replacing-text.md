---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Sökning och ersättning av text är en process där en del text hittas och uppdateras med en annan text. Programmerare använder denna process för att ändra eller uppdatera data baserat på vissa kriterier.

# Hur du gör:
Så här söker och ersätter du text i PowerShell:

```PowerShell
$text = "Hej, jag heter John Doe"
$newText = $text -replace 'John Doe', 'Jane Doe'
Write-Output $newText
```
Ovanstående kod ändrar `John Doe` till `Jane Doe`. Output blir:

```PowerShell
"Hej, jag heter Jane Doe"
```

För att söka och ersätta text med hjälp av ett reguljärt uttryck, använd `-replace` operator på följande sätt:

```PowerShell
$text = "Jag tycker om 7äpplen och 3päron"
$newText = $text -replace '\d', 'tio'
Write-Output $newText
```

Output blir: 

```PowerShell
"Jag tycker om tiotiotoäpplen och tiotiotopäron"
```

# Djupdykning
Searching och replacing text är inte unikt för PowerShell - det har varit en stapelvara i programmering sedan de tidigaste dagarna. Funktionen är inbyggd i många programmeringsspråk och verktyg, inklusive Perl, Python, Java, och till och med textredigeringsverktyg som Vim och Emacs.

Ett alternativ till `-replace` operatören i PowerShell är `String.Replace()` funktionen. Du kan använda det så här:

```PowerShell
$text = "Hej, jag heter John Doe"
$newText = $text.Replace('John Doe', 'Jane Doe')
Write-Output $newText
```
Observera att `String.Replace()` funktionen i PowerShell är skiftskänslig, vilket innebär att det kommer att matcha exakta bokstäver med deras storlek (stora eller små bokstäver).

 `-replace` operator är mer kraftfull eftersom den stöder regular expressions, vilket gör att du kan matcha och ersätta mer komplexa textmönster.

# Se Även
- Officiell Microsoft-dokumentation om `-replace` operator: [här](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1#replacement-operator)

- Lär dig mer om regular expressions i PowerShell: [här](https://www.regular-expressions.info/powershell.html)

- MSDN artikel om String.Replace() funktion: [här](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0)