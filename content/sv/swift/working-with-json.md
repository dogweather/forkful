---
title:                "Swift: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Varför
JSON är en av de vanligaste datatyperna som används inom Swift programmering. Genom att lära sig hur man arbetar med JSON, kan du enkelt hantera data från olika källor och integrera det i dina appar eller projekt.

## Hur man gör
Att arbeta med JSON i Swift är en relativt enkel process, tack vare SwiftyJSON-biblioteket. Här är ett exempel på hur du kan läsa in och bearbeta JSON-data:
```Swift
let jsonString = """
{
  "name": "John",
  "age": 27,
  "hobbies": ["reading", "hiking", "cooking"],
  "address": {
    "street": "Main Street",
    "city": "Stockholm"
  }
}
"""

guard let jsonData = jsonString.data(using: .utf8) else {
  return
}

do {
  let json = try JSON(data: jsonData)
  let name = json["name"].stringValue
  let age = json["age"].intValue
  var hobbies = [String]()
  for (_, hobbyJSON) in json["hobbies"] {
    let hobby = hobbyJSON.stringValue
    hobbies.append(hobby)
  }
  let street = json["address"]["street"].stringValue
  let city = json["address"]["city"].stringValue
  print("Personen \(name) är \(age) år gammal och bor på \(street) i \(city). Hen har följande hobbies:", hobbies)
} catch {
  print("Kunde inte bearbeta JSON-data.")
}
```

Detta exempel visar hur man kan läsa in och bearbeta en enkel JSON-sträng. SwiftyJSON gör det enkelt att komma åt värden inom JSON-strukturen, även när det finns inbäddade objekt och arrayer.

## Djupdykning
För att arbeta mer avancerat med JSON i Swift finns det flera bibliotek tillgängliga, som hjälper till med olika aspekter som validering, parsing och serialisering. Du kan också undersöka möjligheter till att använda Swifts inbyggda Codable-protokoll för att enkelt omvandla JSON-data till Swift-objekt och vice versa.

Det är också viktigt att förstå skillnaderna mellan JSON och andra datatyper, som XML, och när det är lämpligt att använda respektive. Det finns många resurser online som kan hjälpa dig att fördjupa dina kunskaper om JSON och dess användning inom Swift programmering.

## Se också
- [SwiftyJSON](https://github.com/SwiftyJSON/SwiftyJSON)
- [Codable](https://developer.apple.com/documentation/swift/codable)
- [JSON vs XML: Which is Better for Modern Web Design?](https://www.imaginovation.net/blog/json-vs-xml-which-is-better-for-modern-web-design/)