---
title:                "Stor bokstavsättning av en sträng"
html_title:           "Swift: Stor bokstavsättning av en sträng"
simple_title:         "Stor bokstavsättning av en sträng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
At capitalizing en sträng betyder helt enkelt att göra den första bokstaven till en stor bokstav och resten av bokstäverna till små bokstäver. Programmers brukar göra detta för att göra text mer läsbar och för att följa konventioner inom programmering.

## Så här gör du:
Det är enkelt att capitalizing en sträng i Swift med hjälp av metoden `capitalized`. Se nedan för exempel på kod och det genererade output:

```Swift
let sträng = "det här är en teststräng"
print(sträng.capitalized)
```
Output:
`Det här är en teststräng`

Du kan också använda `prefix(1)` för att göra första bokstaven till en stor bokstav och `suffix(from: 1)` för att göra resten av bokstäverna till små:
```Swift
let sträng = "det här är en annan teststräng"
print(sträng.prefix(1).capitalized + sträng.suffix(from: 1).lowercased())
```
Output:
`Det här är en annan teststräng`

## Djupdykning:
Att capitalizing en sträng är en vanlig konvention inom programmering, särskilt när man namnger variabler eller funktioner. Det finns dock alternativ som t.ex. att använda snake_case där ord separeras med understreck istället för att ha alla ord ihopslagna med stor eller liten bokstav. Det finns också möjlighet att anpassa capitalization efter språkregler med hjälp av metoden `capitalized(with: Locale)`. Själva implementationen av capitalization är beroende av språket som används.

## Se även:
- [Swift String Documentation](https://developer.apple.com/documentation/swift/string) 
- [Capitalization in Programming](https://www.benjaminwiederkehr.com/explained/capitalization-in-programming/)