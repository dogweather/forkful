---
title:                "Javascript: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Sending av HTTP-forespørsel med grunnleggende autentisering er en grunnleggende handling i nettutvikling og brukes til å validere brukerens identitet ved å inkludere autentiseringsinformasjon i hver forespørsel. Dette er spesielt viktig for sikre handlinger som krever autentisering.

## Hvordan

Du kan sende en HTTP-forespørsel med grunnleggende autentisering ved å bruke "fetch" -metoden i JavaScript. Først må du definere URL-adressen til serveren du ønsker å sende forespørselen til, samt valgfrie parametere som metode og tilleggsinformasjon. Deretter må du inkludere autentiseringsinformasjonen i et "Authorization" -header ved å kode brukernavnet og passordet i Base64-format.

```Javascript
const url = "https://api.example.com/users";
const options = {
    method: "GET",
    headers: {
        "Authorization": "Basic " + btoa("username:password"),
    },
};

fetch(url, options)
    .then(response => response.json())
    .then(data => console.log(data));
```

I dette eksempelet bruker vi "GET" -metoden og sender forespørselen til "https://api.example.com/users". Vi inkluderer også brukernavnet og passordet vårt i Base64-format ved å bruke metoden "btoa". Når responsen er mottatt, konverterer vi den til JSON og logger den til konsollen.

## Dypdykk

HTTP-forespørsler med grunnleggende autentisering er basert på å sende autentiseringsinformasjon i et "Authorization" -header i forespørselen. Dette headeret inneholder ordet "Basic" etterfulgt av et mellomrom og deretter brukernavnet og passordet i Base64-format. Base64 er en metode for å representere binær data som tekst, og det er en vanlig måte å kode autentiseringsinformasjon for HTTP-forespørsler.

Det er viktig å merke seg at grunnleggende autentisering er ikke en sikker autentiseringsmetode, da informasjonen kan lett dekodes av en tredjepart. Derfor bør man bruke andre autentiseringsmetoder for å sikre sensitive data og handlinger.

## Se også

- [fetch() - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [HTTP Authentication - MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Base64 Encoding - MDN](https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/btoa)