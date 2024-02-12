---
title:                "Hämta aktuellt datum"
aliases:
- /sv/vba/getting-the-current-date.md
date:                  2024-02-01T21:54:41.733519-07:00
model:                 gpt-4-0125-preview
simple_title:         "Hämta aktuellt datum"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/vba/getting-the-current-date.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad och Varför?

I Visual Basic för Applikationer (VBA) är det en vanlig uppgift att hämta aktuellt datum, vilket möjliggör för programmerare att dynamiskt arbeta med datum i deras makron eller applikationer. Denna funktionalitet är avgörande för operationer som loggning, tidsstämpling av transaktioner eller att göra datum-baserade beräkningar.

## Hur gör man:

Att hämta det aktuella datumet i VBA är enkelt, med hjälp av `Date`-funktionen, medan `Now`-funktionen ger både det aktuella datumet och tiden. Så här kan du arbeta med båda:

```vb
Sub GetCurrentDate()
    ' Använda Date-funktionen för att få det aktuella datumet
    Dim currentDate As Date
    currentDate = Date
    Debug.Print "Aktuellt Datum: "; currentDate
    
    ' Använda Now-funktionen för att få det aktuella datumet och tiden
    Dim currentDateTime As Date
    currentDateTime = Now
    Debug.Print "Aktuellt Datum och Tid: "; currentDateTime
End Sub
```

När du kör detta makro kommer `Debug.Print`-metoden att utmatningen av det aktuella datumet och det aktuella datumet och tiden till det Omedelbara fönstret i VBA-editorn. Till exempel:

```
Aktuellt Datum: 2023-04-12
Aktuellt Datum och Tid: 2023-04-12 15:45:22
```

Kom ihåg att datumformatet kan variera baserat på användarens dators systeminställningar.

## Fördjupning

`Date`- och `Now`-funktionerna omsluter komplexiteten med att hantera datum och tid i Visual Basic för Applikationer, och erbjuder en applikationsnivå-abstraktion som gör det enkelt och intuitivt att arbeta med datum. Historiskt sett har hanteringen av datum och tid i programmering varit fylld med utmaningar, inklusive att hantera olika tidszoner, sommartidändringar och olika datumformat.

I VBA förlitar sig dessa funktioner på det underliggande systemets datum och tid, vilket innebär att de påverkas av användarens lokaliseringsinställningar och systeminställningar. Det är ett tveeggat svärd som säkerställer konsistens med användarens miljö men också kräver noggrann hantering av lokaliseringsjusteringar och tidszonsanpassningar i globala applikationer.

Även om VBA:s datum- och tidsfunktioner är fullt lämpliga för många applikationer, särskilt inom omfånget av Office-automatisering, kanske de saknar precisionen eller detaljnivån som krävs för mer komplexa applikationer som system för högfrekvent handel eller vetenskapliga simulationer. I sådana fall kan andra programmeringsmiljöer eller språk som Python eller C# erbjuda mer sofistikerade bibliotek för datum- och tidshantering.

Ändå, för den stora majoriteten av uppgifter som involverar datum och tider i kontexten av Excel, Word eller andra Office-applikationer, erbjuder VBA:s `Date`- och `Now`-funktioner en balans av enkelhet, prestanda och användarvänlighet som är svår att slå.
