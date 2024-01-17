---
title:                "Å sende en http-forespørsel"
html_title:           "Swift: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Noe som er viktig i programmering verden er å sende HTTP forespørsler. HTTP står for Hyper Text Transfer Protocol, og det er den protokollen som brukes for kommunikasjon på internett. Programmerere gjør dette for å hente eller sende data til en server, for eksempel å hente informasjon fra en nettside eller sende data til en database.

# Hvordan:
For å sende en HTTP forespørsel i Swift, kan vi bruke den innebygde URLSession klassen. Først må vi opprette en URL, enten fra en nettside eller en annen kilde, og deretter opprette en URLRequest med denne URLen. Så kan vi bruke URLSession sin dataTask metode til å sende forespørselen og få tilbake en respons.

```Swift
let url = URL(string: "https://www.example.com") // Opprette en URL for nettsiden
var request = URLRequest(url: url!) // Opprette en URLRequest med denne URLen
request.httpMethod = "GET" // Sette en HTTP metode, for eksempel GET eller POST
let session = URLSession.shared // Opprette en URLSession
let task = session.dataTask(with: request) { // Bruke dataTask metoden for å sende forespørselen
    (data, response, error) in
    if let result = NSString(data: data!, encoding: String.Encoding.utf8.rawValue) { // Konvertere dataen til en lesbar form
        print(result) // Skrive ut resultatet
    }
}
task.resume() // Starte forespørselen
```
Output:
```
<html>
    <head>
        <title>Example Domain</title>
    </head>
    <body>
        <h1>Example Domain</h1>
        <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
        <p><a href="https://www.iana.org/domains/example">More information...</a></p>
    </body>
</html>
```

# Dypdykk:
HTTP ble utviklet på 90-tallet, og er fortsatt den mest brukte protokollen for kommunikasjon på internett. Det finnes også alternative protokoller som HTTPS, som bruker kryptering for å sikre sikker kommunikasjon. Implementasjonen av HTTP i Swift er basert på Foundation frameworket, som er et sett med API-er for å håndtere nettverkskall og annen funksjonalitet.

# Se også:
- [Apple Documentation on HTTP requests in Swift](https://developer.apple.com/documentation/foundation/url_loading_system/making_http_and_https_requests)
- [W3Schools tutorial on HTTP requests](https://www.w3schools.com/tags/ref_httpmethods.asp)
- [Webopedia definition of HTTP](https://www.webopedia.com/TERM/H/HTTP.html)