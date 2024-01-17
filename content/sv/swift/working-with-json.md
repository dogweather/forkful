---
title:                "Arbeta med json"
html_title:           "Swift: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med JSON är ett vanligt förekommande uppgift för programmerare. JSON (JavaScript Object Notation) är ett format för att representera datastrukturer i en läsbar och kompakt form. Det används ofta för att överföra data mellan en server och en webbapplikation.

## Så här gör du:
Ett vanligt användningsområde för JSON inom Swift är att hämta och hantera data från en webbtjänst. Här är ett enkelt exempel på hur man kan läsa in JSON-data från en URL:

```Swift
let url = URL(string: "http://example.com/data.json")

if let jsonData = try? Data(contentsOf: url!) {
    let json = try? JSONSerialization.jsonObject(with: jsonData, options: [])
    
    if let data = json as? [String: Any] {
      // gör något med datan här
    }
}
```

Ett annat vanligt scenarium är att skicka data till en server i JSON-format. Här är ett exempel på hur man kan konvertera en Swift-dictionary till JSON och skicka det som en HTTP-request:

```Swift
let data = ["namn": "Lisa", "ålder": 25]
let jsonData = try? JSONSerialization.data(withJSONObject: data, options: [])

// skapar en HTTP-request 
let url = URL(string: "http://example.com/submit")
var request = URLRequest(url: url!)
request.httpMethod = "POST"

// sätter JSON-data som body för requesten 
request.httpBody = jsonData

// skickar requesten
let task = URLSession.shared.dataTask(with: request) { data, response, error in
    // responsehantering här
}
task.resume()
```

## Djupdykning:
JSON är ett populärt format för att representera data eftersom det är läsbart för både människor och datorer. Det är också lätthanterligt i många olika språk och plattformar. Alternativ till JSON är t.ex. XML och CSV, men dessa anses ofta som mer komplicerade och mindre flexibla.

I Swift finns det inbyggda metoder för att konvertera mellan JSON och Swift-datastrukturer, vilket gör det enkelt att integrera med andra system och plattformar. Det finns också ett flertal tredjepartsbibliotek som tillhandahåller mer avancerade funktioner för att hantera JSON-data.

## Se även:
- [Apple's dokumentation om JSONSerialization](https://developer.apple.com/documentation/foundation/jsonserialization)
- [SwiftyJSON - ett populärt tredjepartsbibliotek för att hantera JSON-data i Swift](https://github.com/SwiftyJSON/SwiftyJSON)
- [JSON vs XML vs CSV - en jämförelse av olika dataformat](https://www.guru99.com/json-vs-xml.html)