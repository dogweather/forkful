---
title:                "Redigera filer på plats med kommandoradsenradare"
aliases: - /sv/powershell/editing-files-in-place-with-cli-one-liners.md
date:                  2024-01-27T16:20:54.320777-07:00
model:                 gpt-4-0125-preview
simple_title:         "Redigera filer på plats med kommandoradsenradare"

tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/editing-files-in-place-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att redigera filer direkt med CLI enradskommandon i PowerShell handlar om att göra direkta ändringar i filer från kommandoraden, utan att behöva öppna dem i en textredigerare. Detta tillvägagångssätt sparar tid och kan vara särskilt praktiskt för batchbehandling eller automatisering av repetitiva redigeringsuppgifter över flera filer.

## Hur gör man:

### Ersätta text i en enskild fil

Låt oss börja med en enkel uppgift: du vill ersätta alla förekomster av "oldtext" med "newtext" i en fil som heter example.txt. Så här skulle du göra det:

```PowerShell
(Get-Content example.txt) -replace 'oldtext', 'newtext' | Set-Content example.txt
```

Den här enradskommandot läser innehållet, utför ersättningen och skriver tillbaka innehållet till originalfilen.

### Redigera flera filer

Vad om du behöver tillämpa samma ändring på flera filer? Här är en metod som använder en loop:

```PowerShell
Get-ChildItem *.txt | ForEach-Object {
  (Get-Content $_) -replace 'oldtext', 'newtext' | Set-Content $_
}
```

Det här kodstycket hittar alla `.txt`-filer i den aktuella katalogen och ersätter "oldtext" med "newtext" i var och en.

### Lägga till innehåll i början eller slutet av filer

Att lägga till eller förbereda innehåll kan också strömlinjeformas:

```PowerShell
# Förbereda
"New first line`n" + (Get-Content example.txt) | Set-Content example.txt

# Lägga till i slutet
(Get-Content example.txt) + "`nNew last line" | Set-Content example.txt
```

Här konkatenerar vi helt enkelt det nya innehållet före eller efter det befintliga innehållet och sparar det tillbaka.

## Fördjupning

Historiskt sett är direktredigering oftare förknippad med Unix-verktyg som `sed` och `awk`. PowerShell, som är en senare aktör, inkluderar inte en dedikerad funktion för direktredigering direkt ur lådan. Detta beror delvis på dess designfilosofi som betonar vikten av objekt över textströmmar, till skillnad från Unix-verktyg som behandlar de flesta inmatningar som text.

Alternativ till PowerShell för denna uppgift inkluderar att använda traditionella Unix-verktyg tillgängliga på Windows genom Cygwin eller Windows Subsystem för Linux (WSL). Dessa verktyg erbjuder ofta en mer koncis syntax för direktredigering på grund av deras textcentrerade design.

När det gäller implementering är det viktigt att notera att PowerShell-ansatsen innebär att läsa hela filen till minnet, göra ändringar och sedan skriva tillbaka den. Även om detta fungerar bra för måttligt stora filer, kan det bli ineffektivt för mycket stora filer. I sådana fall kan man överväga att direkt använda `.NET`-metoder eller att vända sig till alternativa verktyg utformade för att strömma stora mängder data.

Trots dessa överväganden är PowerShell:s flexibilitet och omfattande funktionssätt ett ovärderligt verktyg för att manipulera filer direkt från kommandoraden, speciellt för de som redan är djupt involverade i Windows-ekosystemet eller hanterar plattformsoberoende miljöer.
