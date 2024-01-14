---
title:                "C++: Sending en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending en http-forespørsel med grunnleggende autentisering"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hvorfor

Å sende HTTP-forespørsler med grunnleggende autentisering er viktig for å sikre at kun autoriserte brukere har tilgang til en ressurs eller en tjeneste på nettet. Dette er en standard autentiseringsmetode som krever et brukernavn og passord for å få tilgang til en ressurs.

# Hvordan

For å sende en HTTP-forespørsel med grunnleggende autentisering, må du først opprette en HTTP-forespørsel ved hjelp av en HTTP-bibliotek i C++. Deretter må du legge til autentiseringsheaderen, som består av brukernavnet og passordet, til forespørselen.

```C++
#include <iostream>
#include <cpp-httplib/httplib.h> 

using namespace std; 
using namespace httplib;

int main() {
    // Opprette en HTTP-forespørsel
    Client client("www.example.com");

    // Legge til autentiseringsheaderen
    client.set_basic_auth("brukernavn", "passord");

    // Send forespørselen og lagre svaret
    auto response = client.Get("/ressurs");

    // Skriv ut svaret
    cout << response->body << endl;

    return 0;
}
```

Eksempel output:

```
<html>
    <body>
        <h1>Hilsen fra HTTP-serveren!</h1>
    </body>
</html>
```

# Dypdykk

HTTP-forespørsler med grunnleggende autentisering bruker en Base64-koding for å kryptere brukernavnet og passordet før det sendes i en HTTP-header. Dette gjør at informasjonen ikke kan leses av uautoriserte brukere. Det er viktig å merke seg at denne autentiseringsmetoden har sine begrensninger og bør bare brukes på sikre nettsteder.

# Se også

- [HTTP-bibliotek for C++](https://github.com/yhirose/cpp-httplib)
- [Hva er grunnleggende autentisering for HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basic_access_authentication)
- [Andre autentiseringsmetoder for HTTP](https://www.digitalocean.com/community/tutorials/how-to-use-http-basic-authentication-with-nginx-on-ubuntu-14-04)