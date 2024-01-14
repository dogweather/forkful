---
title:    "Swift: Lese en tekstfil"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

I denne bloggposten skal vi se på hvordan man kan lese en tekstfil ved hjelp av Swift-programmeringsspråket. Å lese en tekstfil kan være nyttig for å hente inn data fra en ekstern kilde eller for å lese filer som er generert av andre programmer. Ved å lære hvordan man kan lese en tekstfil, vil du kunne utvide dine programmeringsferdigheter og få mer fleksibilitet i dine prosjekter.

# Hvordan lese en tekstfil i Swift

For å lese en tekstfil i Swift, trenger vi å først opprette en instans av klassen `FileHandle`. Denne klassen lar oss åpne og lese filer i vårt program. Deretter trenger vi å spesifisere banen til filen vi ønsker å lese ved hjelp av `URL`-strukturen.

```Swift
if let fileURL = URL(string: "sti/til/filen.txt") {
    let fileHandle = FileHandle(forReadingAtPath: fileURL.path)
}
```

Nå har vi en instans av `FileHandle`-klassen som er koblet til filen vi ønsker å lese. Vi kan bruke denne instansen til å lese innholdet i filen i en `Data`-variabel. Vi kan også bruke `String`-klasseens init-metode for å konvertere `Data`-variabelen til en lesbar tekststreng.

```Swift
if let fileURL = URL(string: "sti/til/filen.txt") {
    let fileHandle = FileHandle(forReadingAtPath: fileURL.path)
    
    if let data = fileHandle?.readData(ofLength: 128) {
        let content = String(data: data, encoding: .utf8)
        print(content)
    }
}
```

I dette eksempelet leser vi de første 128 bytes av filen og skriver innholdet til konsollen. Det er også mulig å lese hele filen ved hjelp av `readDataToEndOfFile()`-metoden.

# Dypdykk i fil-lesing i Swift

Som nevnt tidligere, lar `FileHandle`-klassen oss ikke bare lese tekstfiler, men også åpne og lese andre typer filer som f.eks. bilder eller lydfiler. Vi kan også bruke metoder som `seek(toFileOffset:)` til å manuelt navigere i filen og lese fra en bestemt posisjon.

I tillegg til å bruke `FileHandle`, kan vi også lese tekstfiler ved hjelp av `NSString`- og `NSStringEncoding`-klassene. Denne metoden er litt enklere og kan være nyttig hvis du kun trenger å hente en liten del av filen.

```Swift
if let fileURL = URL(string: "sti/til/filen.txt") {
    if let content = try? NSString(contentsOf: fileURL, encoding: String.Encoding.utf8.rawValue) {
        print(content)
    }
}
```

# Se også

- [Offisiell dokumentasjon for filbehandling i Swift](https://developer.apple.com/documentation/foundation/filehandle)
- [Swift Playgrounds for å prøve ut fillesing](https://www.apple.com/swift/playgrounds/)
- [Eksempler på prosjekter der fillesing kan være nyttig](https://www.raywenderlich.com/86135/read-write-file-swift)

Takk for at du leste denne bloggposten om å lese tekstfiler i Swift. Vi håper du har fått en bedre forståelse for hvordan man kan lese filer i programmering og kan implementere dette i dine egne prosjekter. Lykke til!