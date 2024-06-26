---
date: 2024-01-20 17:45:00.781641-07:00
description: "How to: (Slik gj\xF8r du:) Swift gir deg URLSession for nettverkskall.\
  \ Her er en kjapp m\xE5te \xE5 laste ned innhold fra en nettside."
lastmod: '2024-04-05T21:53:42.101045-06:00'
model: gpt-4-1106-preview
summary: "(Slik gj\xF8r du:) Swift gir deg URLSession for nettverkskall."
title: Nedlasting av en nettside
weight: 42
---

## How to: (Slik gjør du:)
Swift gir deg URLSession for nettverkskall. Her er en kjapp måte å laste ned innhold fra en nettside:

```swift
import Foundation

let url = URL(string: "https://example.com")!
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("Client error: \(error.localizedDescription)")
    } else if let httpResponse = response as? HTTPURLResponse, httpResponse.statusCode == 200 {
        if let mimeType = httpResponse.mimeType, mimeType == "text/html",
           let data = data,
           let string = String(data: data, encoding: .utf8) {
            print("Downloaded web page: \(string)")
        } else {
            print("Invalid data or MIME type.")
        }
    } else {
        print("Server error or status code not OK.")
    }
}

task.resume()
```

Eksempel på output:
```
Downloaded web page: <!doctype html>...
```

## Deep Dive (Dypdykk)
Før `URLSession` var vi stuck med `NSURLConnection`, men `URLSession` er mer fleksibel og kraftfull. Du kan bruke `URLSessionConfiguration` for å tweake oppførselen, som datahåndtering og timeout-grenser. Det gir også mulighet for å håndtere oppgaver asynkront i bakgrunnen og støtter nedlastinger med pauser og gjenopptak. Et alternativ til `URLSession` er tredjepartsrammeverk som Alamofire, som legger til ekstra funksjoner og ofte har et enklere API.

## See Also (Se også)
- Apple's URLSession documentation: https://developer.apple.com/documentation/foundation/urlsession
- A guide to networking in Swift with URLSession: https://www.raywenderlich.com/3244963-urlsession-tutorial-getting-started
- Alamofire GitHub page for an alternative networking approach: https://github.com/Alamofire/Alamofire
