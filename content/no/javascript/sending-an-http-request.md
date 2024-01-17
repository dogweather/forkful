---
title:                "Sender en http-forespørsel"
html_title:           "Javascript: Sender en http-forespørsel"
simple_title:         "Sender en http-forespørsel"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel betyr å be om informasjon fra en nettside eller webserver. Dette er en vanlig praksis for programmerere når de trenger å hente data fra en fjern server.

## Hvordan:
```Javascript
fetch('https://api.example.com/posts')
    .then(res => res.json())
    .then(data => console.log(data))
```
**Utdata:**
```Javascript
{
    posts: [
        { title: 'Første innlegg', content: 'Dette er mitt første innlegg'},
        { title: 'Andre innlegg', content: 'Dette er mitt andre innlegg'}
    ]
}
```

## Dykk dypere:
HTTP-forespørsler har vært en viktig del av webutvikling siden internettets begynnelse. Det finnes flere måter å sende en forespørsel på, som for eksempel XMLHttpRequest og axios. I moderne javascript er Fetch API den mest brukte metoden for å sende HTTP-forespørsler.

## Se også:
- [Fetch API Dokumentasjon](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Alternativer til Fetch API](https://blog.jscrambler.com/working-with-fetch/)
- [Historien bak HTTP-forespørsler](https://united-coders.com/analyzing-http-requests-and-responses/)