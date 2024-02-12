---
title:                "Refaktorering"
date:                  2024-02-01T21:59:58.365241-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorering"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/vba/refactoring.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Refaktorisering inom programmering innebär att man ändrar strukturen på koden utan att ändra dess beteende, för att förbättra aspekter som läsbarhet, underhållbarhet eller prestanda. Programmerare refaktoriserar för att göra koden mer effektiv, lättare att förstå, enklare att ändra i framtiden och för att minska risken för buggar.

## Hur man gör:

Betrakta ett grundläggande exempel i Visual Basic for Applications (VBA) där vi har en subrutin som skriver ut detaljer om en anställd. Inledningsvis är koden rörig, svår att underhålla eller utvidga.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    MsgBox "Namn: " & name & vbCrLf & "Ålder: " & age & vbCrLf & "Avdelning: " & department
End Sub
```

Refaktorisering steg 1: Extrahera metod. En av de vanligaste refaktoreringsteknikerna är att ta en specifik del av koden och flytta den till sin egen metod. Detta gör koden mer modulär och lättare att förstå.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    VisaMeddelande name, age, department
End Sub

Private Sub VisaMeddelande(name As String, age As Integer, department As String)
    MsgBox "Namn: " & name & vbCrLf & "Ålder: " & age & vbCrLf & "Avdelning: " & department
End Sub
```

Refaktorisering steg 2: Använd en struktur. Detta steg innebär att använda en datastruktur för att hålla relaterad data, vilket förbättrar kodens klarhet och gör det lättare att skicka runt grupperad data.

```vb
Type Anställd
    name As String
    age As Integer
    department As String
End Type

Sub PrintEmployeeDetails()
    Dim emp As Anställd
    emp.name = "John Doe"
    emp.age = 30
    emp.department = "IT"
    
    VisaMeddelande emp
End Sub

Private Sub VisaMeddelande(emp As Anställd)
    MsgBox "Namn: " & emp.name & vbCrLf & "Ålder: " & emp.age & vbCrLf & "Avdelning: " & emp.department
End Sub
```

Dessa steg transformerar rörig kod till modulär, strukturerad kod, vilket avsevärt förbättrar läsbarhet och underhållbarhet.

## Fördjupning

Konceptet med refaktorisering är lika gammalt som programmering självt, men det var Martin Fowlers bok "Refactoring: Improving the Design of Existing Code" som förde det till huvudströmmen, och betonade dess betydelse i mjukvaruutvecklingsprocessen. I Visual Basic for Applications kan refaktorisering vara något mer utmanande på grund av bristen på inbyggda verktyg som finns i modernare integrerade utvecklingsmiljöer (IDEs) som stöder automatiserad refaktorisering.

Detta minskar dock inte dess betydelse. Även i VBA kan tillämpning av grundläggande refaktoreringstekniker manuellt förbättra kodbasen avsevärt, vilket gör den renare och mer effektiv. Även om VBA kanske inte har samma moderna bekvämligheter kvarstår principerna för god koddesign som universella. Utvecklare som kommer från andra språk kan tycka att den manuella processen är tråkig men kommer utan tvekan att uppskatta fördelarna med att investera tid i att förbättra kodkvaliteten från början.

För mer robusta utvecklingsmiljöer eller när man arbetar med särskilt sofistikerade projekt kan det vara värt att utforska alternativ som erbjuder kraftfullare refaktoreringsverktyg eller konvertera VBA-projekt till ett .NET-språk där Visual Studio erbjuder omfattande stöd för refaktorisering. Ändå är förståelsen och tillämpningen av refaktoreringsprinciper i VBA en värdefull färdighet som understryker betydelsen av att skriva ren, underhållbar kod, oavsett miljön.
