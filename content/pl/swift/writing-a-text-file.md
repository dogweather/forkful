---
title:                "Pisanie pliku tekstowego"
html_title:           "Swift: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest nieodłączną częścią wielu projektów programistycznych. Dzięki nim możemy przechowywać dane w zorganizowanej strukturze i korzystać z nich w naszych programach.

## Jak to zrobić

Tworzenie plików tekstowych w języku Swift jest łatwe i wygodne. Wystarczy użyć wbudowanych metod i klasy FileManager. Poniżej znajdują się przykładowe kody z wyjaśnieniami.

```Swift
// Tworzenie pliku
let fileName = "moj_plik.txt"  // nazwa pliku
let fileContents = "To jest nasza pierwsza linijka tekstu." // zawartość pliku

do { // do-try-catch blok służący do obsługi błędów
    let fileURL = try FileManager.default
        .url(for: .documentDirectory, in: .userDomainMask, appropriateFor: nil, create: true)
        .appendingPathComponent(fileName)

    try fileContents.write(to: fileURL, atomically: true, encoding: .utf8) // zapisujemy zawartość do pliku
} catch {
    print(error.localizedDescription) // w przypadku błędu wyświetlamy opis błędu
}

// Odczytywanie pliku
do {
    let fileURL = try FileManager.default
        .url(for: .documentDirectory, in: .userDomainMask, appropriateFor: nil, create: false)
        .appendingPathComponent(fileName)

    let contents = try String(contentsOf: fileURL, encoding: .utf8) // odczytujemy zawartość pliku
    print(contents) // wyświetlamy zawartość w konsoli
} catch {
    print(error.localizedDescription)
}
```

W powyższych przykładach tworzymy i odczytujemy plik o nazwie "moj_plik.txt" w lokalizacji dokumentów użytkownika. Możemy użyć innej lokalizacji, np. biblioteki lub folderu tymczasowego, w zależności od potrzeb naszego projektu. Warto również zwrócić uwagę na używanie bloku "do-try-catch", który pozwala nam na kontrolowanie i obsługę ewentualnych błędów.

## Głębszy zanurzenie

Pisanie plików tekstowych w języku Swift może być bardziej zaawansowane, gdy potrzebujemy przechowywać struktury danych. W takim przypadku możemy skorzystać z klasy NSCoder, która pozwala nam na serializację i deserializację danych.

```Swift
struct Kolor: Codable {
    var red: Float
    var green: Float
    var blue: Float
    var alpha: Float
}

let kolorNiebieski = Kolor(red: 0, green: 0, blue: 1, alpha: 1)

do {
    let data = try JSONEncoder().encode(kolorNiebieski) // serializacja struktury do postaci danych binarnych
    let fileURL = try FileManager.default
        .url(for: .documentDirectory, in: .userDomainMask, appropriateFor: nil, create: true)
        .appendingPathComponent("kolor.txt")

    try data.write(to: fileURL) // zapisujemy dane do pliku
} catch {
    print(error.localizedDescription)
}

do {
    let fileURL = try FileManager.default
        .url(for: .documentDirectory, in: .userDomainMask, appropriateFor: nil, create: false)
        .appendingPathComponent("kolor.txt")

    let data = try Data(contentsOf: fileURL) // odczytujemy dane z pliku
    let kolor = try JSONDecoder().decode(Kolor.self, from: data) // deserializacja danych do wcześniej zdefiniowanej struktury
    print(kolor) // wyświetlamy zdeserializowane dane
} catch {
    print(error.localizedDescription)
}
```

W powyższym przykładzie używamy klasy JSONEncoder i JSONDecoder do serializacji i deserializacji struktury Kolor z wykorzystaniem standardu JSON. Możemy również skorzystać z innych formatów, np. PropertyList lub NSKeyed