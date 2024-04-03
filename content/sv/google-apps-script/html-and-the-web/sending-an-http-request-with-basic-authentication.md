---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:25.718611-07:00
description: "Att skicka en HTTP-f\xF6rfr\xE5gan med grundl\xE4ggande autentisering\
  \ inneb\xE4r att koda ett anv\xE4ndarnamn och l\xF6senord i en f\xF6rfr\xE5gningsheader\
  \ f\xF6r att f\xE5 tillg\xE5ng\u2026"
lastmod: '2024-03-13T22:44:37.438277-06:00'
model: gpt-4-0125-preview
summary: "Att skicka en HTTP-f\xF6rfr\xE5gan med grundl\xE4ggande autentisering inneb\xE4\
  r att koda ett anv\xE4ndarnamn och l\xF6senord i en f\xF6rfr\xE5gningsheader f\xF6\
  r att f\xE5 tillg\xE5ng till skyddade resurser."
title: "Skicka en HTTP-beg\xE4ran med grundl\xE4ggande autentisering"
weight: 45
---

## Vad och Varför?

Att skicka en HTTP-förfrågan med grundläggande autentisering innebär att koda ett användarnamn och lösenord i en förfrågningsheader för att få tillgång till skyddade resurser. Programmerare använder denna metod för autentisering på serversidan, för att integrera med API:er som kräver grundläggande autentisering för operationer som datahämtning eller publicering av innehåll.

## Hur gör man:

I Google Apps Script använder du tjänsten `UrlFetchApp` tillsammans med en base64-kodad autentiseringsheader för att skicka en HTTP-förfrågan med grundläggande autentisering. Här är en steg-för-steg-guide:

1. **Koda inloggningsuppgifter**: Först, koda ditt användarnamn och lösenord i base64. Google Apps Script har ingen inbyggd base64-kodningsfunktion för strängar, så du kommer att använda Utilities.base64Encode för detta ändamål.

```javascript
var username = 'YourUsername';
var password = 'YourPassword';
var encodedCredentials = Utilities.base64Encode(username + ':' + password);
```

2. **Ställ in förfrågningsalternativ**: Med de kodade inloggningsuppgifterna klara, förbered alternativobjektet för HTTP-förfrågan, inklusive metoden och headrarna.

```javascript
var options = {
  method: 'get', // eller 'post', 'put', beroende på dina behov
  headers: {
    'Authorization': 'Basic ' + encodedCredentials
  }
  // ytterligare alternativ som 'muteHttpExceptions' för felhantering kan läggas till här
};
```

3. **Gör förfrågan**: Använd metoden `UrlFetchApp.fetch` med mål-URL:en och alternativobjektet.

```javascript
var url = 'https://example.com/api/resource';
var response = UrlFetchApp.fetch(url, options);
Logger.log(response.getContentText());
```

Exempel på utdata vid en lyckad förfrågan kommer att variera beroende på API:ets svar. För ett API baserat på JSON kan du se något liknande:

```
{"status":"Success","data":"Här är resursdatan..."}
```

Se till att du hanterar möjliga HTTP-fel genom att kontrollera svarskoden eller använda alternativet `muteHttpExceptions` för en mer kontrollerad felhantering.

## Fördjupning

Att skicka en HTTP-förfrågan med grundläggande autentisering har varit en standardmetod i många programmeringsspråk för att komma åt webbaserade resurser som kräver autentisering. I kontexten av Google Apps Script erbjuder `UrlFetchApp` ett enkelt sätt att utföra dessa HTTP-förfrågningar, inklusive de som kräver autentisering. Inkluderingen av grundläggande referenser i förfrågningsheadrarna är en enkel men effektiv metod, men den kommer med säkerhetsvarningar, främst eftersom referenserna skickas i klartext, bara base64-kodade, vilket enkelt kan avkodas om de avlyssnas.

För förbättrad säkerhet rekommenderas alternativ som OAuth 2.0, särskilt när det hanteras känsliga data eller operationer. Google Apps Script har inbyggt stöd för OAuth 2.0 med `OAuth2`-biblioteket, vilket förenklar processen att autentisera mot tjänster som stöder detta protokoll.

Trots sina säkerhetsbegränsningar förblir grundläggande autentisering mycket använd för enkla eller interna applikationer som inte är exponerade för internet i stort. Det är enkelt att implementera, eftersom det bara kräver en enda förfrågan med korrekt inställda headrar, vilket gör det till ett attraktivt alternativ för snabba integrationer eller för API:er där högre säkerhetsmetoder inte är tillgängliga. Dock uppmanas programmerare att överväga säkerhetsimplikationerna och utforska säkrare alternativ när de finns tillgängliga.
