---
title:                "ह्ट्मल का पार्सिंग"
html_title:           "Swift: ह्ट्मल का पार्सिंग"
simple_title:         "ह्ट्मल का पार्सिंग"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Kyun

Kya aap ek Web Developer hain aur aapko kisi website se data extract karna hai? Ya phir aapko ek website par apna algorithm implement karna hai? Iske liye aapko wahan ki HTML code ko parse karna hoga. Swift mein HTML parsing bahut easy hai aur is article mein hum aapko iske kuch tareeke batayenge.

## Kaise

Sabse pehle, aapko Swift ke HTML parsing libraries ko install karna hoga. Sabse popular hai SwiftSoup, jo aapko HTML ka parse karne mein madad karta hai. Chaliye dekhte hain kaise hum iska use kar sakte hain:

```Swift
import SwiftSoup

let html = "<html><head><title>SwiftSoup Demo</title></head><body><h1>Welcome to SwiftSoup</h1><p>This is a demo of SwiftSoup</p></body></html>"

do {
  let doc = try SwiftSoup.parse(html)
  let title = try doc.title()
  print("Title: \(title)")
  let heading = try doc.select("h1").text()
  print("Heading: \(heading)")
  let paragraph = try doc.select("p").text()
  print("Paragraph: \(paragraph)")
} catch Exception.Error(let type, let message) {
  print("Error type: \(type) and message: \(message)")
} catch {
  print("Error: \(error)")
}
```

Is code mein, humne pehle HTML ko string mein liya aur fir usse SwiftSoup ke help se parse kiya. Uske baad, humne title, heading aur paragraph ko extract kiya aur output diya. Aap apne according HTML code ko select kar sakte hain aur uska data extract kar sakte hain.

## Deep Dive

HTML parsing mein ek important concept hai tags. SwiftSoup mein aap tags ko select kar sakte hain aur unke dusre attributes ko bhi access kar sakte hain. Jaise ki, `<img>` tag mein `src` attribute hota hai jo image ki URL deta hai. Aap is attribute ko bhi extract kar sakte hain. Iske alawa, SwiftSoup mein aapko kuch advanced options bhi milte hain jaise ki pagination aur async parsing.

## Dekhein Bhi

- [SwiftSoup Documentation](https://github.com/scinfu/SwiftSoup)
- [HTML Parsing Tutorial in Swift](https://dev.to/ericstringerdev/html-parsing-in-swift--5059)
- [Demo project for HTML parsing in Swift](https://github.com/tarunon/HTMLParserDemo)