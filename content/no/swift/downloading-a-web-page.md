---
title:                "Å laste ned en nettside"
html_title:           "Swift: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville noen ønske å laste ned en nettside? Vel, det kan være flere grunner til det. Kanskje du vil ha en sikkerhetskopi av en nettside, eller kanskje du ønsker å vise den offline. Uansett årsak, lar Swift deg enkelt laste ned en nettside med bare noen få linjer med kode.

## Hvordan

For å laste ned en nettside i Swift, trenger vi bare å bruke URLSession-klassen og dens dataTask-metode. La oss se på et enkelt eksempel:

```Swift
let url = URL(string: "https://www.dinside.no")
let task = URLSession.shared.dataTask(with: url!) { (data, response, error) in
    guard let data = data else {
        print("Ingen data funnet: \(String(describing: error))")
        return
    }
    print("Nettside nedlastet!")
}
task.resume()
```

I dette eksempelet oppretter vi først en URL-objekt for nettsiden vi vil laste ned. Deretter bruker vi URLSession-klassen til å starte en dataTask. Når nedlastingen er fullført, vil "Nettside nedlastet!" bli skrevet ut.

Ettersom vi brukte en lukketuresyntaks med "dataTask" vil vi også få tilgang til den nedlastede nettsidenes data gjennom "data"-variablen. Dette lar deg gjøre mer avanserte operasjoner som å lagre nettsiden som en HTML-fil.

## Dykk dypere

Hvis du ønsker å dykke dypere og få mer kontroll over nedlastingsprosessen, så kan du bruke URLSessionDelegate og dens metoder. Dette lar deg blant annet håndtere eventuelle feil som oppstår under nedlastingen og sjekke nedlastingsprogresjonen.

Det er også verdt å merke seg at URL-metoden vi brukte i eksempelet ovenfor bare støtter nedlastinger av HTTP- og HTTPS-nettsider. Hvis du ønsker å laste ned fra andre protokoller, må du bruke en annen metode, for eksempel en filURL.

## Se også

- [URLSession dokumentasjon](https://developer.apple.com/documentation/foundation/urlsession)
- [Mer informasjon om nedlasting av nettsider i Swift](https://www.hackingwithswift.com/example-code/strings/how-to-download-data-from-a-url)