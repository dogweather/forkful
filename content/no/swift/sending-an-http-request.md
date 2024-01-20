---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel handler om å be en server om dataene den har. Dette gjøres av programmerere fordi det skaffer data fra eksterne kilder som vi kan bruke i våre apps.

## Hvordan Gjør vi Det:

Med Swift kan vi på en enkel måte sende HTTP-forespørsler ved hjelp av `URLSession`. Her er en grunnleggende forespørsel:

```Swift
let url = URL(string: "https://example.com")!
let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
   if let error = error {
      print("Error: \(error)")
   } else if let data = data {
      let str = String(data: data, encoding: .utf8)
      print("Received data: \n\(str ?? "")")
   }
}
task.resume()
```
I eksempelet over begynner vi en ny oppgave som ber om data fra "https://example.com". Deretter skriver vi ut dataene hvis vi får dem, og skriver ut feilen hvis vi får det.

## Gå Dypt:

Historisk sett har `NSURLConnection` vært standardvalget for å sende HTTP-forespørsler i Swift. Men `URLSession` har tatt over fordi det tilbyr mer fleksibilitet og flere funksjoner.

Det finnes alternativer hvis du vil ha enda mer makt over nettverkshåndteringen din. Biblioteker som Alamofire kan være mer egnet for mer komplekse nettverksoperasjoner, men for de fleste formål vil `URLSession` være mer enn nok.

Angående implementasjonsdetaljer er det verdt å merke seg at en `URLSessionDataTask` (som opprettet av `URLSession.shared.dataTask(with:completionHandler:)`) ikke utfører arbeidet sitt før du kaller `.resume()` på den. Hvis du glemmer å ringe denne, vil ikke nettverksanropet ditt bli gjort.

## Se Også:

- Apple Developer Documentation om URLSession: https://developer.apple.com/documentation/foundation/urlsession

- Alamofire, et kraftig nettverksbibliotek for Swift: https://github.com/Alamofire/Alamofire

- Mer info om HTTP-forespørsler: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview