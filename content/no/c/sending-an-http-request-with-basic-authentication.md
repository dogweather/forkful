---
title:                "C: Sending en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending en http-forespørsel med grunnleggende autentisering"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hvorfor

Å sende en HTTP-forespørsel med grunnleggende autentisering er nyttig når du trenger å sikre at bare autoriserte brukere har tilgang til en ressurs på en nettside. Dette kan være nyttig for å beskytte brukernavn og passord eller annen sensitiv informasjon.

# Slik gjør du det

For å sende en HTTP-forespørsel med grunnleggende autentisering i C, må du først inkludere "curl / curl.h" og "string.h" biblioteker i koden din. Deretter initialiserer du en curl struct med "curl_easy_init ()" funksjonen og setter URL-adressen for forespørselen. Du må også angi autentiseringsinformasjonen ved hjelp av "curl_easy_setopt ()" funksjonen.

Her er et eksempel på kode som sender en HTTP-forespørsel med grunnleggende autentisering og skriver ut svaret:

```C
CURL *curl;
CURLcode res;
curl = curl_easy_init();
if (curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "EXAMPLE_URL");
    curl_easy_setopt(curl, CURLOPT_USERNAME, "BRUKERNAVN");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "PASSORD");
    res = curl_easy_perform(curl);
    
    if (res != CURLE_OK)
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
    else
        printf("Svar fra server: \n%s\n", res.data);
    
    curl_easy_cleanup(curl);
}
```

Eksempelutgangen vil se noe slikt ut:

```
Svar fra server:
<!DOCTYPE html>
<html>
  <head>
  ...
</html>
```

# Dykk dypere

For å forstå mer om hvordan HTTP-forespørsler og autentisering fungerer, bør du undersøke de ulike delene av koden og lese mer om protokollene. Det kan også være nyttig å se på forskjellige tilgjengelige autentiseringsmetoder og hvordan de kan implementeres i C.

# Se også

- [Curl dokumentasjon](https://curl.haxx.se/libcurl/c/)
- [HTTP-protokollen](https://www.w3.org/Protocols/rfc2616/rfc2616.html)
- [Basic Authentication på MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)