---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel er å be en server om en ressurs, for eksempel en nettside. Som programmerere gjør vi dette for å hente data, oppdatere data, slette data, etc.

## Hvordan:

Først, vi må inkludere Ethernet-biblioteket.

```Arduino
#include <Ethernet.h>
```

Dernest, definerer vi MAC-adressen og IP-adressen.

```Arduino
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
IPAddress ip(192,168,1,177);
```

Og vi initialiserer Ethernet-serveren.

```Arduino
EthernetServer server(80);
```

Nå kan vi åpne en forbindelse og sende HTTP-forespørselen.

```Arduino
EthernetClient client = server.available();
client.println("GET / HTTP/1.1");
```

Forespørslens resultat vil da være tilgjengelig i klientobjektet.

## Dypere info

HTTP-forespørsler ble opprinnelig brukt i nettlesere for å hente nettsideinnhold. I Arduino kan vi bruke dem til å kommunisere med web-APIer, sende data til skytjenester, etc. Det finnes flere metoder å sende HTTP-forespørsler på, GET og POST er de mest brukte. GET brukes til henting av data, POST til sending. I denne opplæringen brukte vi GET.

Detaljer om implementeringen: I vårt eksempel definerte vi en MAC-adresse og IP-adresse. I virkeligheten vil MAC-adressen være unik for det eksakte Ethernet-grensesnittet du bruker, og IP-adressen vil avhenge av nettverkskonfigurasjonen din.

## Se også

- Arduino Ethernet-biblioteket: [link](https://www.arduino.cc/en/Reference/Ethernet)
- Mer informasjon om HTTP-forespørsler: [link](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- Hvordan bruke POST forespørsler med Arduino: [link](https://randomnerdtutorials.com/esp8266-web-client/)