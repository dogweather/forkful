---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel med grunnleggende autentisering innebærer å gi brukernavn og passord for å få tilgang til serverressurser. Våre programmer gjør dette for å sikre trygg og autorisert kommunikasjon.

## Slik gjør du det:

Du kan bruke `Axios`, et populært TypeScript HTTP-klientbibliotek, som støtter grunnleggende autentisering. Installere dependency først:

```TypeScript
npm install axios
```

Nå, her er enkel kode for å sende en GET-forespørsel med grunnleggende autentisering:

```TypeScript
import axios from 'axios';

async function fetchData() {
    const response = await axios.get('http://dinwebside.com', {
        auth: {
            username: 'brukernavn',
            password: 'passord'
        }
    });
    
    console.log(response.data);
}

fetchData();
```

Utdata skal være responsdataene fra serveren.

## Dypdykk:

Grunnleggende autentisering ble først definert i HTTP/1.0-standarder på 90-tallet som en måte å kontrollere tilgang til webservressurser på. Det har stått testen av tid, men det er viktig å merke seg at ved å bruke HTTPS-transmisjon blir dataene kryptert og er trygge fra "man-in-the-middle"-angrep.

Alternativer som OAuth og tokenbasert autentisering har blitt mer populære fordi de gir mer sikkerhet og fleksibilitet.

Når Axios sender en HTTP-forespørsel med grunnleggende autentisering, kodes brukernavn og passord til et "Authorization"-header i base64 format.

## Se også:

For mer informasjon, sjekk linkene nedenfor:

1. TypeScript (Offisiell Dokumentasjon): https://www.typescriptlang.org/docs/
2. Axios (GitHub Repository): https://github.com/axios/axios
3. MDN Web Docs (HTTP Autentisering): https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication