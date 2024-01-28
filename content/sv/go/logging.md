---
title:                "Loggning"
date:                  2024-01-26T01:04:25.074085-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggning"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/logging.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Loggning handlar om att hålla en register över händelser, tillstånd och dataflöden inom en app. Programmerare gör det för att diagnostisera buggar, övervaka prestanda och spåra appens operativa hälsa—vilket i stort sett gör det till en mjukvaruekvivalent till en svart låda i flygplan.

## Hur man gör:
I Go kan loggning hanteras på flera sätt, från standardbibliotekets `log`-paket till tredjepartsbibliotek såsom `logrus` och `zap`. Här är ett enkelt exempel som använder det inbyggda `log`-paketet:

```Go
package main

import (
	"log"
	"os"
)

func main() {
	// Skapa en loggfil
	logFile, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer logFile.Close()

	// Ställ in loggutmatningen till filen
	log.SetOutput(logFile)

	// Logga några händelser
	log.Println("Startar applikationen...")
	// ... applikationslogik här ...
	log.Println("Applikationen avslutades framgångsrikt.")
}
```

Om du kör denna kod kommer du inte att se något i terminalen eftersom allt går till `app.log`. Här är en titt på vad du skulle hitta inuti den loggfilen:

```
2023/01/02 15:04:05 Startar applikationen...
2023/01/02 15:05:01 Applikationen avslutades framgångsrikt.
```

## Fördjupning
Loggning inom programmering går tillbaka till de tidigaste datorerna, där ingenjörer bokstavligen skulle hitta buggar (nattflyn, för att vara exakt) mosade i hårdvaran, och de loggade detta! Tiden fram till idag, och loggning har blivit ett sofistikerat sätt att förstå vad som händer inuti komplexa system.

Även om `log`-paketet i Go är ganska enkelt, kan det räcka för grundläggande applikationer. Men i kontexten av moderna distribuerade system, eller när du behöver mer nyanserad kontroll över din loggutmatning (som olika nivåer av allvarlighet), kan det vara värt att utforska mer robusta lösningar.

Tredjepartsloggningsbibliotek som `logrus` och `zap` erbjuder strukturerad loggning, vilket innebär att du kan logga komplexa datatyper som JSON, vilket gör det enklare att tolka loggar, särskilt i samband med logghanteringssystem som ELK Stack eller Splunk.

När man överväger implementeringen av en loggningsstrategi är det också viktigt att tänka på prestandapåverkan. Loggbibliotek med hög prestanda är optimerade för att minska effekten på applikationens genomströmning och latens. Till exempel skryter `zap` med sin blixtsnabba, låga allokering design, vilket kan vara avgörande för realtidssystem.

Utöver olika bibliotek är även loggningsformat och standarder värda att notera. Strukturerade loggningsformat som JSON kan vara enormt kraftfulla när de används tillsammans med loggbehandlingssystem. Å andra sidan är vanliga textloggar läsbara för människor men mer utmanande att tolka programmatiskt.

## Se också
För att fördjupa dig i Gos loggningsfunktioner kan dessa resurser vara användbara:

- Go-bloggen om loggning: https://blog.golang.org/logging
- `logrus`, en strukturerad loggare för Go: https://github.com/sirupsen/logrus
- `zap`, en snabb, strukturerad, nivåindelad loggare: https://github.com/uber-go/zap
- ELK Stack (Elasticsearch, Logstash, Kibana) för logganalys: https://www.elastic.co/what-is/elk-stack
- En jämförelse av Gos loggningsbibliotek: https://www.loggly.com/blog/benchmarking-5-popular-golang-logging-libraries/
