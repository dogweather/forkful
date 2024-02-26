---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:55.222065-07:00
description: "Att skicka en HTTP-beg\xE4ran i Visual Basic for Applications (VBA)\
  \ inneb\xE4r att man programmatiskt tillg\xE5r webbresurser eller webbtj\xE4nster\
  \ genom att g\xF6ra\u2026"
lastmod: '2024-02-25T18:49:36.034269-07:00'
model: gpt-4-0125-preview
summary: "Att skicka en HTTP-beg\xE4ran i Visual Basic for Applications (VBA) inneb\xE4\
  r att man programmatiskt tillg\xE5r webbresurser eller webbtj\xE4nster genom att\
  \ g\xF6ra\u2026"
title: "Skicka en HTTP-f\xF6rfr\xE5gan"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-begäran i Visual Basic for Applications (VBA) innebär att man programmatiskt tillgår webbresurser eller webbtjänster genom att göra förfrågningar över HTTP. Programmerare gör detta för att hämta data, interagera med online-API:er eller skicka formulär programmatiskt från inom sina VBA-aktiverade applikationer som Excel, Access eller anpassade VBA-lösningar.

## Hur:

Nyckeln till att skicka en HTTP-begäran i VBA är att använda `Microsoft XML, v6.0`-biblioteket (eller äldre versioner, beroende på ditt system). Se först till att denna referens är aktiverad i ditt projekt genom att gå till Verktyg > Referenser i VBA-redigeraren och kryssa för `Microsoft XML, v6.0`.

Så här skickar du en enkel HTTP GET-begäran:

```vb
Dim httpRequest As Object
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")

With httpRequest
    .Open "GET", "https://api.example.com/data", False
    .send
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Fel: " & .Status & " - " & .statusText
    End If
End With
```

För en POST-begäran, där vi behöver skicka data (t.ex. JSON) till en server:

```vb
Dim httpRequest As Object, postData As String
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")
postData = "{""key"":""value""}"

With httpRequest
    .Open "POST", "https://api.example.com/submit", False
    .setRequestHeader "Content-Type", "application/json"
    .send postData
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Fel: " & .Status & " - " & .statusText
    End If
End With
```

Exempelutdata för en lyckad begäran kan vara en JSON-sträng eller en HTML-sida, beroende på API:et eller webbsidan du interagerar med:

```
{"data": "Detta är svaret från servern"}
```

## Fördjupning

Metoden som visas använder `MSXML2.XMLHTTP`-objektet, en del av Microsoft XML Core Services (MSXML). Det introducerades för att erbjuda VBA-utvecklare ett sätt att utföra XML-baserade operationer och blev med tiden ett vanligt verktyg för HTTP-begäran, även när man inte arbetar direkt med XML-data. Trots sin ålder förblir det ett pålitligt alternativ för enkla webbinteraktioner i VBA.

Däremot saknar VBA och dess mekanismer för HTTP-begäran robustheten och flexibiliteten som finns i moderna programmeringsmiljöer. Till exempel är hantering av asynkrona begäran eller arbete inom applikationer som kräver avancerade HTTP-funktioner (som websockets eller server-sända händelser) utanför VBA:s räckvidd. När man arbetar med mer komplexa webbintegrationsprojekt använder utvecklarna ofta externa bibliotek eller verktyg, eller till och med automatiserar webbläsarbeteende genom webbskrapningstekniker, även om dessa är kringgående lösningar snarare än lösningar.

Språk och miljöer som Python med dess `requests`-bibliotek eller JavaScript som körs på Node.js erbjuder mycket kraftfullare och mångsidigare HTTP-begäransmöjligheter direkt ur lådan, inklusive asynkrona operationer, enklare hantering av JSON och omfattande stöd för olika webbteknologier. Utvecklare som är djupt rotade i Microsoft-ekosystemet kan överväga att övergå till PowerShell eller C# för uppgifter som kräver mer sofistikerad webbinteraktion, och utnyttja .NET:s omfattande nätverksprogrammeringsfunktioner.

Således, även om VBA:s HTTP-begäranskapacitet är tillräcklig för enkla förfrågningar och datahämtningsuppgifter, blir det avgörande att utforska alternativ när ditt projekts krav utvecklas mot det komplexa och moderna webblandskapet.
