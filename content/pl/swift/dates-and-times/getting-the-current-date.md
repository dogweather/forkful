---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:19.542755-07:00
description: "Pobieranie aktualnej daty w j\u0119zyku Swift polega na u\u017Cyciu\
  \ klasy `Date`, aby uzyska\u0107 dost\u0119p do daty i czasu, w kt\xF3rym aplikacja\
  \ jest uruchamiana.\u2026"
lastmod: '2024-03-13T22:44:35.765220-06:00'
model: gpt-4-0125-preview
summary: "Pobieranie aktualnej daty w j\u0119zyku Swift polega na u\u017Cyciu klasy\
  \ `Date`, aby uzyska\u0107 dost\u0119p do daty i czasu, w kt\xF3rym aplikacja jest\
  \ uruchamiana."
title: Pobieranie aktualnej daty
weight: 29
---

## Co i dlaczego?
Pobieranie aktualnej daty w języku Swift polega na użyciu klasy `Date`, aby uzyskać dostęp do daty i czasu, w którym aplikacja jest uruchamiana. Programiści muszą pobierać aktualną datę z wielu powodów, począwszy od timestampowania zdarzeń, wykonywania obliczeń związanych z datami, po wyświetlanie dat i godzin w interfejsie użytkownika.

## Jak to zrobić:
Framework `Foundation` Swifta dostarcza klasę `Date`, co sprawia, że uzyskanie aktualnej daty i godziny jest proste. Oto podstawowy przykład, jak uzyskać aktualną datę:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

To spowoduje wyświetlenie czegoś takiego:

```
2023-04-12 07:46:23 +0000
```

Format wyjściowy odpowiada standardowi ISO 8601, używając strefy czasowej UTC. Jednakże możesz chcieć sformatować tę datę do wyświetlania. W takim przypadku na ratunek przychodzi klasa `DateFormatter`:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

Przykładowe wyjście może wyglądać tak:

```
12 kwietnia 2023, 10:46:23
```

Zwróć uwagę, że format wyjściowy będzie się różnić w zależności od ustawień regionalnych urządzenia, na którym jest uruchamiany kod.

Dla projektów wymagających bardziej skomplikowanej manipulacji datami, wielu programistów Swifta korzysta z bibliotek stron trzecich, takich jak `SwiftDate`. Oto jak możesz użyć `SwiftDate`, żeby uzyskać aktualną datę w określonej strefie czasowej i formacie:

Najpierw dodaj `SwiftDate` do swojego projektu za pomocą SPM, CocoaPods lub Carthage. Następnie:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

To może wyjść:

```
2023-04-12 09:46:23
```

Używając `SwiftDate`, możesz łatwo manipulować datami i godzinami dla różnych stref czasowych i ustawień regionalnych, co sprawia, że skomplikowane zadania związane z obsługą dat w twoich aplikacjach Swift są prostsze.
