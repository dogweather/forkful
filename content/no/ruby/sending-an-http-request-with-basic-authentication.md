---
title:                "Ruby: Sending en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending en http-forespørsel med grunnleggende autentisering"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hvorfor
HTTP forespørsler med grunnleggende autentisering er en viktig del av webutvikling. Dette lar deg sikre at informasjon som sendes mellom klient og server er beskyttet og kun tilgjengelig for godkjente brukere. Uten grunnleggende autentisering, kan sensitiv informasjon bli eksponert og nettstedet ditt kan bli truet av uautorisert tilgang.

# Hvordan å gjøre det
For å sende en HTTP forespørsel med grunnleggende autentisering, trenger du å bruke Ruby og en HTTP-klient som "Net::HTTP". Følgende kode eksempel viser en enkel måte å utføre dette på:

```ruby
require 'net/http'
uri = URI('http://www.example.com/path')
req = Net::HTTP::Get.new(uri)
req.basic_auth 'username', 'password'
res = Net::HTTP.start(uri.hostname, uri.port) do |http|
  http.request(req)
end
puts res.body if res.is_a?(Net::HTTPSuccess)
``` 
I dette eksempelet bruker vi "Net::HTTP::Get" klassen for å definere en GET forespørsel mot en URI. Deretter legger vi til grunnleggende autentiseringsopplysninger med "basic_auth" metoden, og sender til slutt forespørselen med "Net::HTTP.start" metoden. Hvis forespørselen er vellykket, vises kroppen av responsen på skjermen.

# Dykk dypere
Nå som du har fått en grunnleggende forståelse av hvordan du sender en HTTP forespørsel med grunnleggende autentisering, la oss se på noen flere ting du bør være klar over. Først må du sørge for at nettstedet ditt støtter grunnleggende autentisering. Du kan sjekke dette ved å se etter "WWW-Authenticate" header i responsen. Hvis det ikke vises, må du kanskje aktivere autentisering på serveren din.

Det er også viktig å merke seg at passordet blir sendt som en base64-kodet streng, ikke i klartekst. Dette er for å beskytte passordet mot uvedkommende tilgang, men det er viktig å ikke stole på dette som en eneste form for sikkerhet. Det anbefales å bruke HTTPS i stedet for HTTP for å kryptere forespørselen og beskytte sensitiv informasjon.

# Se også
- [Net::HTTP dokumentasjon](https://ruby-doc.org/stdlib-2.6.5/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTP grunnleggende autentisering](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme) (engelsk)
- [Ruby sin base64 klasse](https://ruby-doc.org/stdlib-2.6.5/libdoc/base64/rdoc/Base64.html)