---
title:                "Sända en http-förfrågan"
html_title:           "Swift: Sända en http-förfrågan"
simple_title:         "Sända en http-förfrågan"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan är när en programmerare ber om data från en server. Det kan vara för att hämta information från en API eller för att köra en webbsida. Detta är en viktig del av webbprogrammering och används för att få tillgång till olika resurser online.

## Så här gör du:
```Swift
// Skapa en URL
let url = URL(string: "https://api.com/users")

// Skapa en förfrågan
var request = URLRequest(url: url!)

// Ange metod, t.ex. GET eller POST
request.httpMethod = "GET"

// Skapa en URL session
let session = URLSession(configuration: .default)

// Utför förfrågan
let task = session.dataTask(with: request) { data, response, error in
  // Kontrollera eventuella fel
  if let error = error {
    print("Error: \(error)")
    return
  }
  
  // Kontrollera svarsstatus
  guard let httpResponse = response as? HTTPURLResponse,
        (200...299).contains(httpResponse.statusCode) else {
    print("Fel svar från servern.")
    return
  }
  
  // Konvertera svar till Swift-typ
  if let data = data {
    let decodedData = String(data: data, encoding: .utf8)
    print("Svar från servern: \(decodedData)")
  }
}

// Starta förfrågan
task.resume()
```

Detta är en grundläggande kod för att skicka en HTTP-förfrågan. Det finns många andra detaljer som kan läggas till beroende på dina behov och vilken typ av data du vill hämta.

## Djupdykning:
Det är viktigt att förstå historiska kontexter och alternativ när det gäller att skicka HTTP-förfrågningar. HTTP-protokollet har funnits sedan början av internetets uppkomst och har utvecklats över tid. Det finns också andra sätt att skicka förfrågningar, som till exempel via Sockets. Det är viktigt att förstå dessa detaljer för att välja den bästa metoden för ditt projekt.

## Se även:
Här är några länkar som kan hjälpa dig att lära dig mer om att skicka HTTP-förfrågningar:

- [Apple Developer Documentation - URL Loading System](https://developer.apple.com/documentation/foundation/url_loading_system)
- [W3 Schools - HTTP Requests](https://www.w3schools.com/js/js_http_requests.asp)
- [NSHipster - NSURLSession](https://nshipster.com/nsurlsession/)