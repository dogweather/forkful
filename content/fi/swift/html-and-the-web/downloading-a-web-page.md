---
date: 2024-01-20 17:44:57.283863-07:00
description: "Ladataan nettisivua ohjelmassa, eli haetaan sen sis\xE4lt\xF6\xE4 koodilla.\
  \ Se on hy\xF6dyllist\xE4, kun tarvitaan automaattista tiedonkeruuta tai halutaan\u2026"
lastmod: '2024-03-13T22:44:56.905884-06:00'
model: gpt-4-1106-preview
summary: "Ladataan nettisivua ohjelmassa, eli haetaan sen sis\xE4lt\xF6\xE4 koodilla."
title: Verkkosivun lataaminen
weight: 42
---

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
