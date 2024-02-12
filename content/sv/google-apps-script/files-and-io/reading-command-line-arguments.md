---
title:                "Läsa kommandoradsargument"
aliases:
- /sv/google-apps-script/reading-command-line-arguments/
date:                  2024-02-01T21:59:18.812642-07:00
model:                 gpt-4-0125-preview
simple_title:         "Läsa kommandoradsargument"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/google-apps-script/reading-command-line-arguments.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa kommandoradsargument i Google Apps Script är en smula missvisande eftersom Google Apps Script, till skillnad från traditionella kommandoradsgränssnitt i programmeringsspråk som Python eller Node.js, inte inneboende stöder exekvering via kommandorad eller tolkning av argument. Istället simulerar kodare ofta denna process genom egendefinierade funktioner och URL-parametrar när de kör webbappar eller automatiserade uppgifter, vilket möjliggör dynamisk interaktion med scriptets funktionaliteter baserat på användarinmatningar eller fördefinierade parametrar.

## Hur man gör:

För att efterlikna processen att läsa kommandoradsargument i Google Apps Script, särskilt för webbappar, kan du utnyttja frågesträngsparametrar. När en användare tillgår webbappens URL kan du bifoga argument som `?name=John&age=30` och tolka dessa inom din Apps Script-kod. Så här kan du ställa in detta:

```javascript
function doGet(e) {
  var params = e.parameter; // Hämtar frågesträngsparametrarna
  var name = params['name']; // Hämtar 'name'-parametern
  var age = params['age']; // Hämtar 'age'-parametern

  // Exempel på utmatning:
  var output = "Namn: " + name + ", Ålder: " + age;
  return HtmlService.createHtmlOutput(output);
}

// Exempel-URL: https://script.google.com/macros/s/your_script_id/exec?name=John&age=30
```

När du tillgår URL:en med de angivna parametrarna ger scriptet något som liknar:

```
Namn: John, Ålder: 30
```

Detta tillvägagångssätt är betydande för att skapa personliga interaktioner i webbappar eller för att programmerat styra scriptexekveringar.

## Djupdykning

Kommandoradsargument, som förstås i sammanhanget av traditionella programmeringsspråk, frambringar förmågan för script och applikationer att bearbeta körtidsparametrar, vilket möjliggör flexibla och dynamiska kodexekveringar baserade på användarinmatning eller automatiserade processer. Google Apps Script, som är ett molnbaserat script-språk för lättviktsapplikationsutveckling inom Google Workspace-ekosystemet, opererar inte nativt via ett kommandoradsgränssnitt. Istället är dess exekvering till största del händelsedriven eller manuellt utlöst genom Apps Script och Google Workspace UI, eller via webbappar som kan tolka URL-parametrar som pseudo-kommandoradsargument.

Givet denna arkitektoniska skillnad, kan programmerare som kommer från en bakgrund med tungt CLI-användande behöva justera sitt tillvägagångssätt när de automatiserar uppgifter eller utvecklar applikationer i Google Apps Script. Istället för traditionell tolkning av kommandoradsargument, kan utnyttjandet av Google Apps Scripts webb-appfunktionalitet eller till och med anpassade funktioner i Google Sheets för interaktiv databehandling tjäna liknande syften. Även om detta först kan verka som en begränsning, uppmuntrar det till utvecklingen av mer användarvänliga gränssnitt och tillgängliga webbapplikationer, i linje med Google Apps Scripts fokus på sömlös integration och utvidgning av Google Workspace-applikationer.

För scenarier där en närmare emulation av CLI-beteende är av yttersta vikt (t.ex. automatisering av uppgifter med dynamiska parametrar), kunde utvecklare utforska att utnyttja externa plattformar som anropar Google Apps Script-webbappar, med att skicka parametrar genom URL:er som en provisorisk "kommandorads"-metod. Dock, för infödda Google Apps Script-projekt, leder ofta omfamnandet av plattformens händelsedrivna och UI-centrerade modell till mer raka och underhållbara lösningar.
