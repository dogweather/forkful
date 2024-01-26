---
title:                "Loggføring"
date:                  2024-01-26T01:06:36.323715-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggføring"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/logging.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Logging handler om å føre en protokoll over hendelser, tilstander og dataflyt inni en applikasjon. Programmerere gjør dette for å diagnostisere feil, overvåke ytelse og spore applikasjonens driftshelse – noe som omtrent gjør det til programvareekvivalenten til en svart boks i fly.

## Hvordan:
I Go kan logging håndteres på flere måter, fra standardbibliotekets `log`-pakke til tredjepartsbibliotekene som `logrus` og `zap`. Her er et enkelt eksempel som bruker den innebygde `log`-pakken:

```Go
package main

import (
	"log"
	"os"
)

func main() {
	// Opprett en loggfil
	logFile, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer logFile.Close()

	// Sett loggutdata til filen
	log.SetOutput(logFile)

	// Logg noen hendelser
	log.Println("Starter applikasjonen...")
	// ... applikasjonslogikk her ...
	log.Println("Applikasjonen avsluttes vellykket.")
}
```

Hvis du kjører denne koden, vil du ikke se noen utdata i terminalen fordi alt går inn i `app.log`. Her er en titt på hva du ville finne inni den loggfilen:

```
2023/01/02 15:04:05 Starter applikasjonen...
2023/01/02 15:05:01 Applikasjonen avsluttes vellykket.
```

## Dypdykk
Logging i programmering går tilbake til de første datamaskinene, hvor ingeniørene bokstavelig talt ville finne insekter (nattfly, for å være nøyaktig) klemt fast i maskinvaren, og de ville loggføre dem! Hurtig frem til i dag, og logging har blitt en sofistikert måte å forstå hva som skjer innad i komplekse systemer.

Selv om `log`-pakken i Go er ganske enkel, kan den være tilstrekkelig for grunnleggende applikasjoner. Men i konteksten av moderne distribuerte systemer, eller når du trenger mer nyansert kontroll over loggutdataene dine (som ulike nivåer av alvorlighetsgrader), kan det hende du ønsker å utforske mer robuste løsninger.

Tredjeparts loggingbiblioteker som `logrus` og `zap` tilbyr strukturert logging, noe som betyr at du kan loggføre komplekse datatyper som JSON, noe som gjør det enklere å tolke logger, spesielt i kombinasjon med logghåndteringssystemer som ELK Stack eller Splunk.

Når du vurderer implementeringen av en loggstrategi, er det også essensielt å tenke på ytelsesimplikasjoner. Loggingbiblioteker med høy ytelse er optimalisert for å redusere påvirkningen på applikasjonens gjennomstrømning og latenstid. For eksempel skryter `zap` av sin lynraske, lave allokering-design, som kan være avgjørende for sanntidssystemer.

I tillegg til ulike biblioteker, er loggingformater og -standarder også verdt å merke seg. Strukturerte loggingformater som JSON kan være enormt kraftfulle når de brukes i sammenheng med loggprosesseringssystemer. På den andre siden er vanlige tekstlogger menneskelesbare, men vanskeligere å analysere programmatisk.

## Se Også
For å dykke dypere inn i Gos loggingkapasiteter, kan disse ressursene være nyttige:

- Go-bloggen om logging: https://blog.golang.org/logging
- `logrus`, en strukturert logger for Go: https://github.com/sirupsen/logrus
- `zap`, en rask, strukturert og nivådelt logger: https://github.com/uber-go/zap
- ELK Stack (Elasticsearch, Logstash, Kibana) for logganalyse: https://www.elastic.co/what-is/elk-stack
- En sammenligning av Go-loggingbiblioteker: https://www.loggly.com/blog/benchmarking-5-popular-golang-logging-libraries/