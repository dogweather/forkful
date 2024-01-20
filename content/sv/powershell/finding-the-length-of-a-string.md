---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

---

## Vad & Varför?
Att hitta strängens längd handlar om att räkna antalet tecken i en specifik sträng. Programmers gör detta för att kontrollera datainmatning, manipulera strängar, eller för kejsare villkor som kräver exakt stränglängd.

## Hur gör man:
För att hitta en strängs längd i PowerShell, använd ‘Length’ egenskapen av strängobjektet. Här är ett enkelt exempel:

```PowerShell
$sträng = "Hej, Sverige!"
Write-Host $sträng.Length
```
I detta fall kommer output att vara `14`, eftersom det finns 14 tecken i strängen "Hej, Sverige!".

## Djupdykning:
1. Historiskt sammanhang: ‘Length’ egenskapen av ett strängobjekt har använts sedan PowerShell först skapades. Det är en del av .NET-ramverket, vilket PowerShell är byggt på.
  
2. Alternativ: Byte-kodningen i en sträng kan också ha betydelse. Till exempel kan vissa tecken i en sträng vara flerbyte-tecken, vilket kan påverka stränglängden när du räknar i bytes och inte i tecken. I dessa fall kan du använda ‘Encoding’ klassen för att hitta strängens längd i bytes.

```PowerShell
$sträng = "Hej, Sverige!"
$byteLängd = [System.Text.Encoding]::UTF8.GetByteCount($sträng)
Write-Host $byteLängd
```
3. Implementeringsdetaljer: Notera att PowerShell inte gör skillnad mellan enkla och dubbla citationstecken för strängar. Även om du använder enkla citationstecken kommer stränglängden att räknas korrekt.

## Se Även:
1. Microsoft’s dokument om stränglängder: https://docs.microsoft.com/sv-se/dotnet/api/system.string.length?view=net-5.0
2. Diskussion om Stränglängder på StackOverflow: https://stackoverflow.com/questions/2144853/powershell-string-length
3. Guide till PowerShell Strängmanipulation: https://adamtheautomator.com/powershell-string-manipulation/

---