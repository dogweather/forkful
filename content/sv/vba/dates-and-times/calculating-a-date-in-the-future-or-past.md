---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:21.719927-07:00
description: "Att ber\xE4kna ett datum i framtiden eller f\xF6rflutet inneb\xE4r att\
  \ best\xE4mma ett datum som \xE4r ett angivet antal dagar, m\xE5nader eller \xE5\
  r bort fr\xE5n ett givet\u2026"
lastmod: '2024-03-13T22:44:37.757741-06:00'
model: gpt-4-0125-preview
summary: "Att ber\xE4kna ett datum i framtiden eller f\xF6rflutet inneb\xE4r att best\xE4\
  mma ett datum som \xE4r ett angivet antal dagar, m\xE5nader eller \xE5r bort fr\xE5\
  n ett givet\u2026"
title: "Ber\xE4kning av ett datum i framtiden eller f\xF6rflutet"
weight: 26
---

## Vad & Varför?
Att beräkna ett datum i framtiden eller förflutet innebär att bestämma ett datum som är ett angivet antal dagar, månader eller år bort från ett givet datum. Programmerare behöver ofta denna funktionalitet för att automatisera påminnelser, prenumerationer, utgångsdatum och schemaläggning av uppgifter i olika applikationer.

## Hur man gör:
I Visual Basic for Applications (VBA) är den primära funktionen som används för att beräkna framtida eller förflutna datum `DateAdd()`. Denna funktion lägger till ett angivet tidsintervall till ett datum och returnerar ett nytt datum.

Här är ett grundläggande exempel för att lägga till 10 dagar till dagens datum:

```vb
Dim futureDate As Date
futureDate = DateAdd("d", 10, Date) ' Lägger till 10 dagar till dagens datum
Debug.Print futureDate ' Skriver ut något sådant som: 2023-04-20
```

Liknande, för att hitta ett datum 10 dagar i det förflutna:

```vb
Dim pastDate As Date
pastDate = DateAdd("d", -10, Date) ' Drar av 10 dagar från det nuvarande datumet
Debug.Print pastDate ' Skriver ut: 2023-03-31, antagande att idag är 2023-04-10
```

Dessa exempel är ganska enkla. Du kan ersätta `"d"` med andra intervallkoder, som `"m"` för månader och `"yyyy"` för år, för att beräkna olika typer av datumberäkningar. Så här kan du beräkna ett datum ett år i framtiden:

```vb
Dim nextYear As Date
nextYear = DateAdd("yyyy", 1, Date) ' Lägger till 1 år till dagens datum
Debug.Print nextYear ' Skriver ut: 2024-04-10 om idag är 2023-04-10
```

## Djupdykning
Funktionen `DateAdd` har varit en grundläggande del av VBA sedan dess början, härstammande från dess föregångare BASIC. Även om det erbjuder enkelhet för att lägga till eller dra ifrån tidsintervaller från datum, är det viktigt att notera att VBA, inklusive dess datumhanteringsfunktioner, inte alltid kan matcha bekvämligheten eller effektiviteten som finns i nyare programmeringsspråk.

Till exempel erbjuder moderna språk som Python med `datetime`-modulen eller JavaScript med bibliotek såsom `moment.js` och `date-fns` mer intuitiva och kraftfulla sätt för datummanipulation. Dessa alternativ ger bättre stöd för lokalisering, tidszoner och skottår, vilket kan göra dem mer lämpliga för applikationer som kräver precisa datumberäkningar på en global skala.

Dock, för Excel-makron och applikationer som kräver integrering inom Microsoft Office-ekosystemet, är VBA fortfarande ett praktiskt val. Enkelheten i att direkt komma åt och manipulera Excel-data är en signifikant fördel. Dessutom, för de flesta grundläggande datumberäkningarna som schemaläggning och påminnelser, erbjuder `DateAdd()` i VBA en adekvat och okomplicerad lösning. Dess syntax är lätt att förstå för nybörjare, medan dess integration i det bredare Office-sviten av applikationer säkerställer dess relevans i specifika användningsfall.

Sammanfattningsvis, även om alternativa programmeringsspråk kan erbjuda mer moderna tillvägagångssätt till datumberäkning, fungerar `DateAdd()` i VBA som ett bevis på språkets varaktighet i de domäner där det mest behövs.
