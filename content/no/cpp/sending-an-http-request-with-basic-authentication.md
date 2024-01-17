---
title:                "Send en http-forespørsel med grunnleggende autentisering"
html_title:           "C++: Send en http-forespørsel med grunnleggende autentisering"
simple_title:         "Send en http-forespørsel med grunnleggende autentisering"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

HTTP-forespørsler er en metode for å kommunisere med en server ved å be om spesifikke ressurser, for eksempel en nettside. Noen ganger er det viktig å sikre at bare autoriserte brukere har tilgang til disse ressursene. Dette er når sending av en HTTP-forespørsel med grunnleggende autentisering kommer inn i bildet. Dette er en måte å bekrefte identiteten til en bruker ved å inkludere et brukernavn og passord i forespørselen.

# Hvordan å gjøre det?

For å sende en HTTP-forespørsel med grunnleggende autentisering, trenger du en HTTP-client i ditt C++ program. Her er et eksempel med boost::beast bibliotek:

```C++

// inkudere de nødvendige bibliotekene
#include <boost/asio.hpp>
#include <boost/beast.hpp>
#include <iostream>

int main() {
  // opprette en boost::asio::io_context for å håndtere nettverkstilkoblinger
  boost::asio::io_context io_context;

  // opprette en TCP-socket og koble til serveren
  boost::asio::ip::tcp::socket socket(io_context);
  socket.connect(boost::asio::ip::tcp::endpoint(
      boost::asio::ip::make_address("127.0.0.1"), 80));

  // konstruere en HTTP-forespørsel med et brukernavn og passord
  boost::beast::http::request<boost::beast::http::string_body> request;
  request.version = 11;
  request.method(boost::beast::http::verb::get);
  request.target("/protected/resource");
  request.set(boost::beast::http::field::host, "www.example.com");
  request.set(boost::beast::http::field::authorization,
              "Basic dXNlcjE6cGFzc3dvcmQ="); // dette er et eksempel, det faktiske
                                           // brukernavnet og passordet må settes

  // sende forespørsel til serveren
  boost::beast::http::write(socket, request);

  // motta respons og skrive ut til konsollen
  boost::beast::flat_buffer buffer;
  boost::beast::http::response<boost::beast::http::dynamic_body> response;
  boost::beast::http::read(socket, buffer, response);
  std::cout << response << "\n";

  // lukke socketen
  boost::beast::error_code error;
  socket.shutdown(boost::asio::ip::tcp::socket::shutdown_both, error);
}
```

Eksempeloutput: 

```
HTTP/1.1 200 OK
Date: Thu, 01 Jul 2021 12:00:00 GMT
Server: Apache
Content-Type: text/html; charset=UTF-8
Content-Length: 1234

<body>Protected resource</body>
```

# Dykk dypere

Grunnleggende autentisering er en av de eldste formene for autentisering på nettet og ble først definert i HTTP-standarden i 1999. Det er en enkel og effektiv metode, men den mangler sikkerhet siden brukernavn og passord sendes i klartekst over nettverket.

En alternativ måte å autentisere på er ved hjelp av oauth2-protokollen, som bruker tokens i stedet for brukernavn og passord for å bekrefte identitet. For mer informasjon om hvordan du implementerer dette i C++, kan du se dokumentasjonen til Abseil biblioteket.

En viktig detalj å merke seg er at noen servere kan kreve en SSL-tilkobling for å kunne bruke grunnleggende autentisering, så det er viktig å sjekke med servere dokumentasjon eller utvikler før du sender HTTP-forespørselen.

# Se også

[Boost Beast Dokumentasjon](https://www.boost.org/doc/html/beast.html)

[Abseil Biblioteket](https://abseil.io/docs/cpp/)