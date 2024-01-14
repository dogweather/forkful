---
title:                "Swift: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

# Varför

När man vill strukturera och lagra data på ett överskådligt sätt är YAML ett utmärkt val. Det är ett enkelt och lättläst filformat som används inom programmering för att organisera data.

# Hur man använder YAML i Swift

För att använda YAML i Swift behöver man först installera ett externt bibliotek som heter "Yams". Detta gör man enkelt genom att gå till "File" -> "Swift Packages" -> "Add Package Dependency" och sedan söka efter "Yams". Efter att ha installerat biblioteket är man redo att börja använda YAML i sin kod.

```Swift
import Yams

// Skapa en variabel med en YAML-sträng
let yamlString = """
myName: John Doe
age: 30
favoriteColors:
  - blue
  - green
  - red
"""

// Konvertera YAML-strängen till ett Swift-dictionary
let dictionary = try Yams.load(yaml: yamlString) as! [String: Any]

// Hämta värden från dictionary och skriv ut dem
if let name = dictionary["myName"] as? String {
    print("Mitt namn är \(name).")
}

if let age = dictionary["age"] as? Int {
    print("Jag är \(age) år gammal.")
}

if let favoriteColors = dictionary["favoriteColors"] as? [String] {
    print("Mina favoritfärger är \(favoriteColors.joined(separator: ", ")).")
}
```

Output:

> Mitt namn är John Doe.
>
> Jag är 30 år gammal.
>
> Mina favoritfärger är blue, green, red.

# Fördjupning

YAML har en enkel och lättläst syntax, vilket gör det till ett populärt val för att lagra och strukturera data. Det är också ett plattformsoberoende format, vilket innebär att det kan användas på olika operativsystem och programmeringsspråk.

I YAML kan man använda både objekt och listor för att organisera data. Objekt representeras med användning av "nyckel: värde"-par, medan listor representeras med punktlistor. Dessa kan sedan användas för att skapa hierarkier i YAML-filerna.

YAML stödjer också kommentarer, vilket gör det möjligt för utvecklare att förklara och dokumentera sin kod på ett läsbart sätt.

# Se även

- [Yams bibliotek](https://github.com/jpsim/Yams)
- [Officiell YAML-specifikation](https://yaml.org/spec/1.2/spec.html)