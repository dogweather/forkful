---
title:                "Verkkosivun lataaminen"
aliases:
- /fi/swift/downloading-a-web-page.md
date:                  2024-01-20T17:44:57.283863-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Ladataan nettisivua ohjelmassa, eli haetaan sen sisältöä koodilla. Se on hyödyllistä, kun tarvitaan automaattista tiedonkeruuta tai halutaan interaktiivisia toimintoja.

## How to: (Kuinka tehdään:)
Swiftissä ladataan nettisivuja URLSession-entiteetin avulla. Tässä simppeleitä esimerkkejä:

```Swift
import Foundation

// URL:n luonti
guard let url = URL(string: "https://example.com") else {
    print("Invalid URL")
    exit(1)
}

// URLSession käyttö
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("Client error: \(error.localizedDescription)")
        return
    }
    
    guard let httpResponse = response as? HTTPURLResponse,
          (200...299).contains(httpResponse.statusCode) else {
        print("Server error: respnse not in the range 200...299")
        return
    }
    
    if let mimeType = httpResponse.mimeType, mimeType == "text/html",
       let data = data,
       let string = String(data: data, encoding: .utf8) {
        print(string)
    }
}

// Aloitetaan tehtävä
task.resume()
```

## Deep Dive (Sukellus syvyyksiin)
Web-sivujen lataaminen on yleistynyt 1990-luvun alusta, kun WWW yleistyi. Vaihtoehtona URLSessionille on erilaiset kolmansien osapuolten kirjastot, kuten Alamofire. URLSession toimii delegaattipohjaisesti tai sulkeumien (closures) kautta, joten se integroituu hyvin Swiftin kanssa.

## See Also (Katso myös)
- [Apple Developer Documentation: URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Alamofire GitHub Page](https://github.com/Alamofire/Alamofire)
