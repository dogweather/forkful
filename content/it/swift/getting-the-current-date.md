---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:16:50.396601-07:00
simple_title:         "Ottenere la data corrente"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
("## Cosa & Perché?")

Ottenere la data corrente in programmazione significa catturare l'istante esatto in cui il codice viene eseguito. Programmatori lo fanno per timestamp, storico, o funzionalità basate sul tempo come reminder o orologi.

## How to:
("## Come fare:")

Swift rende semplice ottenere la data corrente con `Date()`:

```Swift
import Foundation

let currentDate = Date()
print(currentDate)
```

Output di esempio:

```
2023-03-18 15:15:47 +0000
```

Formatiamo la data (italiano, in Italia):

```Swift
let formatter = DateFormatter()
formatter.dateStyle = .short
formatter.timeStyle = .short
formatter.locale = Locale(identifier: "it_IT")
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

Output di esempio:

```
18/03/23, 16:15
```

## Deep Dive:
("## Approfondimento:")

Historicamente, Swift ha introdotto modi sempre più intuitivi per lavorare con date e tempo. Pre-Swift, Objective-C utilizzava `NSDate`, che è ancora disponibile come classe sottostante per compatibilità.

Alternative includono l'uso di calendari per gestire fusi orari e localizzazioni:

```Swift
var calendar = Calendar.current
calendar.locale = Locale(identifier: "it_IT")
let components = calendar.dateComponents([.year, .month, .day, .hour, .minute], from: currentDate)
```

Per quanto riguarda l'implementazione, `Date()` in Swift usa il tempo in secondi trascorso dall'era UNIX (1 gennaio 1970). È importante considerare i fusi orari quando si lavora con le date.

## See Also:
("## Vedi Anche:")

- Documentazione ufficiale di Swift su `Date`: [Swift Date](https://developer.apple.com/documentation/foundation/date)
- Guida alla localizzazione e formattazione delle date: [Date Formatting](https://nsdateformatter.com)
- Apple Developer, informazioni su `DateComponents`: [DateComponents](https://developer.apple.com/documentation/foundation/datecomponents)
