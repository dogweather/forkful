---
title:                "Ladda ner en webbsida"
html_title:           "Swift: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida betyder att hämta information från en webbsida och visa den på vår enhet. Programmerare gör detta för att hämta och bearbeta data från olika webbkällor, såsom API:er och websidor.

## Så här gör du:
```Swift
if let url = URL(string: "https://www.example.com") { 
    // Skapa en URL för den webbsida du vill ladda ner 
    URLSession.shared.dataTask(with: url) { data, response, error in 
        if let error = error { 
            print("Det gick inte att ladda ner sidan: \(error.localizedDescription)") 
        } else if let data = data { 
            // Bearbeta och visa datan
            print(String(decoding: data, as: UTF8.self)) 
        } 
    }.resume() 
}
```

## Djupdykning:
Att ladda ner en webbsida har varit en viktig del av webbutveckling sedan internet började användas. Innan tillgången av API:er var det vanligt att använda skript och botar för att hämta data från webbsidor. Idag finns det också alternativa sätt att hämta data, som till exempel användning av tredjepartsbibliotek och direkt tillgång till databaser på serversidan.

## Se även:
- [How to: Hämta webbsidor med Swift](https://www.hackingwithswift.com/example-code/networking/how-to-download-a-webpage-with-swift)
- [Swift Network Libraries](https://www.raywenderlich.com/1265222-best-swift-networking-libraries-for-ios-and-macos)