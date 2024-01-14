---
title:                "Bash: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor
HTTP-forespørsler er et viktig konsept innenfor Bash-programmering og kan være nyttig i ulike situasjoner. Å kunne sende en HTTP-forespørsler med Bash gir deg muligheten til å hente data fra en ekstern server og integrere det i dine egne scripts eller verktøy. Dette er spesielt nyttig for automatisering og datainnsamling.

## Hvordan
Å sende en HTTP-forespørsel med Bash er enkelt og kan gjøres med `curl` kommandoen. Her er et eksempel på hvordan du kan hente data fra en ekstern API og lagre det som en variabel i Bash:

```Bash
response=$(curl -s https://example.com/api/data)
echo $response #Dette vil skrive ut innholdet i responsen fra API-et
```

Dette eksempelet bruker `curl` til å sende en GET-forspørsel til en ekstern server og lagre resultatet i variabelen "response". Du kan også legge til flere parametere, som for eksempel en spesifikk URL eller header, for å tilpasse forespørselen etter behov.

## Deep Dive
HTTP-forespørsler følger et bestemt format som inkluderer en forespørselslinje, headers og en valgfri body. Det finnes forskjellige metoder for å sende en forespørsel, som GET, POST, PUT og DELETE, som kan utføres ved hjelp av `curl` kommandoen.

En viktig del av å sende HTTP-forespørsler er å kunne håndtere resultatet. Du kan bruke verktøy som `jq` for å parse JSON-data eller `grep` for å filtrere responsen. Det er også viktig å være klar over eventuelle feil eller problemer i responsen, og håndtere dem ved å bruke vilkår eller unntak.

## Se også
- [Bash docs: curl](https://www.gnu.org/software/bash/manual/html_node/curl.html)
- [Bash docs: jq](https://www.gnu.org/software/bash/manual/html_node/jq.html)
- [Linuxize: cURL command in Bash with examples](https://linuxize.com/post/curl-command-examples/)