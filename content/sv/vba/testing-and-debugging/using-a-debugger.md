---
title:                "Att använda en debugger"
aliases:
- /sv/vba/using-a-debugger.md
date:                  2024-02-01T22:04:06.162324-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda en debugger"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/vba/using-a-debugger.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att använda en felsökare i Visual Basic for Applications (VBA) innebär att köra din kod steg för steg för att inspektera dess exekveringsflöde och variabeltillstånd. Denna process är avgörande för att identifiera och åtgärda fel i din kod, vilket i slutändan säkerställer att den fungerar som förväntat.

## Hur man gör:

I VBA är felsökaren en integrerad del av Visual Basic Editorn (VBE). Så här kan du utnyttja den:

1. **Att sätta brytpunkter**: Klicka i vänstermarginalen bredvid den kodrad du är intresserad av, eller placera din markör på raden och tryck på F9. Detta meddelar VBA att pausa exekveringen när den når denna punkt.

    ```vb
    Sub DebugExample()
        Dim counter As Integer
        For counter = 1 To 5
            Debug.Print counter ' Sätt brytpunkt här
        Next counter
    End Sub
    ```

    När koden utförs kommer den att pausa vid raden `Debug.Print counter`, vilket gör att du kan inspektera variabelvärden.

2. **Stega in (F8)**: Med det här kommandot utför du din kod ett uttalande i taget, och går in i eventuella anropade procedurer. Det är användbart för att spåra hur din kod och funktioner interagerar.

3. **Bevakningsfönster**: Använd Bevakningsfönstret för att övervaka värdena på variabler eller uttryck. Om en variabel inte är inom omfång, kommer Bevakningsfönstret att indikera det. Högerklicka på en variabel > Lägg till bevakning.

4. **Omedelbart fönster (Ctrl+G)**: Detta fönster är särskilt användbart för att testa uttryck eller modifiera variabelvärden medan du felsöker. Skriv `?variabelnamn` för att skriva ut en variabels aktuella värde, eller tilldela ett nytt värde med `variabelnamn = nyttVärde`.

    ```vb
    ' I Omedelbart fönster
    ?counter ' Skriver ut det nuvarande värdet på counter
    counter = 3 ' Ställer in värdet på counter till 3
    ```

5. **Exempel på utdata**:

    När du når brytpunkten och utför rad för rad med F8 kan Omedelbart fönster visa något sådant här:

    ```
    counter = 1
    counter = 2
    counter = 3
    ```

    Här har vi manuellt frågat efter variabeln `counter` efter varje iteration.

## Fördjupning:

Felsökaren i VBA, om än robust, är en del av en bredare tradition av felsökningsverktyg i programmeringsspråk, och har utvecklats avsevärt från sina tidiga föregångare. Introducerat med de första versionerna av VBA, syftade det till att förse utvecklare med ett enkelt men kraftfullt uppsättning verktyg för kodinspektion och korrigering. Med tiden har förbättringar inkluderat villkorsbaserade brytpunkter, förbättrade bevakningsmöjligheter och integrering med Excel-gränssnittet för mer intuitiv datainspektion.

Jämfört med moderna Integrerade Utvecklingsmiljöer (IDEer) som Visual Studio eller Eclipse, kan VBA:s felsökningsverktyg verka grundläggande. Dessa moderna IDEer erbjuder mer sofistikerade funktioner såsom realtidsvariabelinspektion, avancerade brytpunkter och integrerade ramverk för enhetstestning. Medan dessa alternativ ger mer omfattande felsökningsupplevelser, förblir enkelheten och direktigheten i VBA:s felsökare väl lämpade för det specifika sammanhanget av automatisering och skriptande inom Microsoft Office-applikationer.

För programmerare vana vid dessa moderna miljöer kan anpassningen till VBA:s felsökningsverktyg kräva en förändring i strategi. Ändå är de grundläggande principerna för att inspektera variabler, stega igenom kod och observera körningstidens beteende universella. Med övning blir VBA:s felsökare ett oumbärligt verktyg för att säkerställa att dina automatiseringsskript fungerar felfritt inom Office-ekosystemet.
