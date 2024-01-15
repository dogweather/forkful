---
title:                "Att arbeta med json"
html_title:           "Swift: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

Om du arbetar med webbutveckling, mobilapplikationer eller backend-system är det troligt att du kommer att stöta på JSON-filer. JSON, eller JavaScript Object Notation, är ett populärt format för att överföra och lagra data. Genom att lära dig hur man arbetar med JSON i Swift kommer du kunna hantera data på ett mer effektivt sätt och skapa mer dynamiska applikationer.

## Hur man gör det

Att arbeta med JSON i Swift är ganska enkelt och det finns många användbara inbyggda funktioner som hjälper till att hantera JSON-data. Först behöver du importera Foundation-ramverket, som innehåller de nödvändiga klasserna för att hantera JSON.

```Swift
import Foundation
```

För att läsa en JSON-fil och omvandla den till en Swift-dictionary kan du använda `JSONSerialization`-klassen. Låt oss säga att vi har en JSON-fil som innehåller information om olika böcker:

```Swift
let jsonFile = "{
  \"books\": [
    {
      \"title\": \"Sagan om ringen\",
      \"author\": \"J.R.R. Tolkien\",
      \"genre\": \"Fantasy\"
    },
    {
      \"title\": \"Hundraåringen som klev ut genom fönstret och försvann\",
      \"author\": \"Jonas Jonasson\",
      \"genre\": \"Humor\"
    }
  ]
}"
```

För att läsa in denna fil och omvandla den till en dictionary kan du använda följande kod:

```Swift
let data = jsonFile.data(using: .utf8)!
do {
  if let jsonDictionary = try JSONSerialization.jsonObject(with: data, options: .allowFragments) as? [String: Any] {
    let books = jsonDictionary["books"] as! [[String: Any]]
    for book in books {
      let title = book["title"] as! String
      let author = book["author"] as! String
      let genre = book["genre"] as! String
      print("Title: \(title), Author: \(author), Genre: \(genre)")
    }
  }
} catch {
  print("Error parsing JSON: \(error)")
}
```

Detta kommer att ge följande utmatning:

```
Title: Sagan om ringen, Author: J.R.R. Tolkien, Genre: Fantasy
Title: Hundraåringen som klev ut genom fönstret och försvann, Author: Jonas Jonasson, Genre: Humor
```

För att konvertera en Swift-dictionary till JSON kan du använda `JSONSerialization` igen:

```Swift
let bookDictionary: [String: Any] = [
  "title": "Harry Potter och de vises sten",
  "author": "J.K. Rowling",
  "genre": "Fantasy"
]

do {
  let jsonData = try JSONSerialization.data(withJSONObject: bookDictionary, options: .prettyPrinted)
  if let jsonString = String(data: jsonData, encoding: .utf8) {
    print(jsonString)
  }
} catch {
  print("Error converting dictionary to JSON: \(error)")
}
```

Detta kommer att ge följande utmatning:

```Swift
{
  "title": "Harry Potter och de vises sten",
  "author": "J.K. Rowling",
  "genre": "Fantasy"
}
```

## Djupdykning

Det finns många andra användbara funktioner och klasser för att arbeta med JSON i Swift, som till exempel `Codable`-protokollet som gör det enklare att omvandla mellan Swift-objekt och JSON. Det är också möjligt att arbeta med JSON över nätverk genom att använda `URLSession` och `DataTask`.

Se även:

- [Working with JSON in Swift](https://developer.apple.com/swift/blog/?id=37)
- [Codable in Swift: Beyond the Basics](https://www.raywenderlich.com/1168358-codable-in-swift)
- [Using Codable with URLRequests and URLSessions in Swift 4](https://medium.com/@nimjea/using-codable-with-urlrequests-and-urlsessions-in-swift-4-b332f7c5e10e)