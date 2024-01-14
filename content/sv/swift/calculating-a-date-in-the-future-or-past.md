---
title:    "Swift: Beräkna ett datum i framtiden eller förflutna"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför 

Att beräkna ett datum i framtiden eller det förflutna är ett vanligt problem inom programmering, särskilt när det kommer till att hantera datum och tid i appar eller webbplatser. Genom att förstå hur man kan beräkna dessa datum kan du enkelt implementera det i dina projekt och säkerställa korrekta datum och tider för användarna. 

## Hur man gör det 

För att beräkna ett datum i framtiden eller det förflutna i Swift finns det flera olika metoder som kan användas beroende på dina specifika behov. Här är några exempel: 

```Swift 
// Skapa ett datum för idag 
let idag = Date() 

// Lägga till 1 vecka till dagens datum 
let enVeckaFranNu = Calendar.current.date(byAdding: .weekOfYear, value: 1, to: idag) 

// Subtrahera 2 månader från dagens datum 
let tvaManaderTillbaka = Calendar.current.date(byAdding: .month, value: -2, to: idag) 

// Beräkna ett datum med ett specifikt datumkomponenter 
let datumKomponenter = DateComponents(year: 2021, month: 12, day: 25) 
let jul = Calendar.current.date(from: datumKomponenter) 

// Formatera datumet som en sträng 
let datumFormatter = DateFormatter() 
datumFormatter.dateStyle = .medium 
print(datumFormatter.string(from: jul!)) // Resultat: 25 dec 2021 
``` 

## Djupdykning 

När du beräknar datum i framtiden eller förflutna är det viktigt att förstå att vissa månader har olika antal dagar och att olika tidszoner kan påverka resultatet. Det är därför viktigt att använda rätt metod och att beakta dessa faktorer för att få ett korrekt resultat. Det kan också vara användbart att lära sig om hur Swifts Calendar-klass fungerar och vilka andra metoder som finns tillgängliga för att hantera datum och tid. 

## Se även 

* [Swifts officiella dokumentation för datum och tid](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html) 
* [Förbättra prestandan vid arbete med datums](https://medium.com/orangesoftvard/optimizing-date-calculations-in-swift-ef0167308883) 
* [Datumkomponenter i Swift](https://oleb.net/blog/2011/11/working-with-date-and-time-in-cocoa-part-5-date-components/)