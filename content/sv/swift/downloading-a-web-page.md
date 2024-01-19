---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Ladda ner en Webbplats sida med Swift

## Vad & Varför?
Att ladda ner en webbplats sida innebär ganska enkelt att hämta dess kod och innehåll för att sedan använda eller bearbeta lokalt. Programmerare gör det ofta för att skrapa data, testa webbplatser, eller skapa offline-versioner av sidor.

## Så här gör du:
Med URLSession, URLSessionTask och URLRequest kan du ladda ner en webbplats sida i Swift. Här är ett exempel:

```Swift
import Foundation

let url = URL(string: "https://www.dittwebbplats.se")!
let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
    if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Fått data:\n\(str ?? "")")
    }
} 

task.resume()
```

När du kör det här, bör du se sidans HTML-kod skrivs ut i konsolen.

## Djupdykning
Historiskt sett har vi gått från långsamma nedladdningar via modem, till snabbare bredbandsanslutningar, vilket möjliggör mer komplexa och innehållsrika webbsidor. 

Alternativ till Swift för att ladda ner en webbplats sida inkluderar språk som Python, Java och JavaScript, alla med sina egna bibliotek för webbkommunikation.

När det gäller implementeringsdetaljer har Swift URLSession, URLSessionTask och URLRequest objekt för lättanvänd webbkommunikation. URLSession hanterar en grupp relaterade nätverksåtgärder. URLRequest kapslar in detaljerna i den åtgärd du vill genomföra.

## Se även 
- Apple Docs URLSession: https://developer.apple.com/documentation/foundation/urlsession
- Apple Dev Networking: https://developer.apple.com/network/
- Stanford Swift Course on iTuneU: https://itunes.apple.com/us/course/developing-ios-10-apps-with-swift/id1198467120