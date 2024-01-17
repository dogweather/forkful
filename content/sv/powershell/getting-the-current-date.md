---
title:                "Att få den aktuella datumet"
html_title:           "PowerShell: Att få den aktuella datumet"
simple_title:         "Att få den aktuella datumet"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

Vad & Varför?
Att få den nuvarande datumen refererar till att ta reda på det aktuella datumet och klockslaget på din dator eller server. Det är ett viktigt verktyg för programmerare att använda för att spåra och logga händelser, samt för att schemalägga uppgifter och automatiska processer.

 Hur gör man:
I PowerShell finns det flera sätt att få den nuvarande datumen. Ett enkelt sätt är att använda cmdleten `Get-Date`, som returnerar det aktuella datumet i standardformatet. Se till exempel:

```
PowerShell Get-Date
```

Detta kommer att ge dig ett resultat som liknar detta:

```
 fredag 25 september 2020 14:09:04
```

Du kan också använda parametern `-DisplayHint` för att få en mer lättläst version av datumet, till exempel:

```
PowerShell Get-Date -DisplayHint Date
```

Denna kod ger dig bara datumet utan tidsinformationen, som visas nedan:

```
fredag 25 september 2020
```

Du kan också anpassa formatet på datumet med hjälp av `-Format` parametern. Till exempel kan du ange ett format för att visa datumet i ISO-format genom att använda strängen `yyyy-MM-dd`. Här är ett exempel på hur du kan få det aktuella datumet i detta format:

```
PowerShell Get-Date -Format "yyyy-MM-dd"
```

Detta kommer att ge dig resultatet som nedan:

```
2020-09-25
```

Djupdykning:
Get-Date cmdleten har funnits sedan början av PowerShell och är en del av .NET Framework. Det finns också andra sätt att få den nuvarande datumen, såsom genom att använda .NET-klassen ```System.DateTime```. Detta ger mer precision och anpassningsmöjligheter för datumet, men kräver mer kod för att åstadkomma det.

Se även:
För mer information om hur du använder Get-Date cmdleten, kan du alltid använda `Get-Help` cmdleten i PowerShell för att få dokumentation och exempel. Du kan också utforska .NET-klassen `System.DateTime` för att lära dig mer om alternativ för att få den nuvarande datumen.