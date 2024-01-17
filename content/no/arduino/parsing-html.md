---
title:                "Parsing av html"
html_title:           "Arduino: Parsing av html"
simple_title:         "Parsing av html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/parsing-html.md"
---

{{< edit_this_page >}}

**Hva & Hvorfor?**

Parsing HTML er en prosess der data fra en nettside blir analysert og strukturert slik at de kan brukes av en datamaskin. Dette er viktig fordi det gjør det mulig å hente ut spesifikke data fra en nettside, for eksempel prisene på produktene eller værvarselet for en bestemt by.

**Hvordan:**

For å parse HTML på Arduino trenger du en Ethernet-skjerm og en internettforbindelse. Først må du bruke ```Ethernet.begin(mac, ip)``` for å sette opp forbindelsen. Deretter må du definere en ```client``` for å koble til nettsiden du vil hente data fra. Du kan bruke ```client.connect(ip, port)``` for å knytte deg til serveren på nettsiden. Når forbindelsen er etablert, kan du bruke ```client.println("GET / HTTP/1.1")``` for å be om nettsidens HTML-kode. Deretter bruker du ```client.readStringUntil('\r')``` for å lese nettsidens HTML-kode linje for linje. Du kan bruke ```Serial.println()``` for å skrive ut koden på seriell monitor, eller du kan bruke ```String```-variabler for å lagre informasjonen og bruke den videre i koden din.

**Dypdykk:**

Parsing HTML har vært en viktig del av webutvikling siden de tidlige dagene av internett. Før HTML-styler og moderne webdesign ble vanlig, var HTML primært brukt til å generere enkel tekst og lenker. Men med utviklingen av moderne nettsteder som er rike på innhold og data, ble det nødvendig å kunne tolke og strukturere HTML-kode for å hente ut spesifikke data. Alternativer til å parse HTML inkluderer å bruke API-er eller webskraping-verktøy.

Når du parser HTML på Arduino, er det viktig å være oppmerksom på at det kan være utfordrende å hente ut bestemte data fra nettsiden, siden HTML-kode kan variere fra nettside til nettside. Du må også være nøye med å følge riktig syntaks og elementer når du bruker HTML-parseren på Arduino.

**Se også:**

For mer informasjon om parsing av HTML på Arduino, kan du se på dokumentasjonen for Ethernet-biblioteket og prøve ut forskjellige eksempler for å øve deg på å hente ut data fra forskjellige nettsider. Du kan også se på forumet på Arduino-nettsiden for å få hjelp og råd fra andre brukere.