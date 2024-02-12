---
title:                "Kontrollera om en katalog existerar"
aliases:
- /sv/swift/checking-if-a-directory-exists/
date:                  2024-02-03T19:08:50.404768-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kontrollera om en katalog existerar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns i filsystemet är avgörande för att hantera filstrukturer från dina Swift-applikationer. Denna uppgift möjliggör för utvecklare att verifiera närvaron av kataloger innan de försöker läsa från eller skriva till dem, och undviker därmed möjliga körningsfel.

## Hur:

Swifts Foundation-ramverk tillhandahåller `FileManager`-klassen, som har metoder för att hantera filsystemet. Du kan använda `FileManager` för att kontrollera om en katalog finns. Här är en kodsnutt för hur man gör detta:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Katalogen finns")
} else {
    print("Katalogen finns inte")
}
```

Detta kontrollerar dock både filer och kataloger. Om du specifikt vill verifiera att en katalog finns, behöver du skicka en pekare till ett Booleskt värde i `isDirectory`:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("Katalogen finns")
} else {
    print("Katalogen finns inte")
}
```

### Användning av ett tredjepartsbibliotek

Som det är nu, kräver inte kontrollen av om en katalog finns i Swift vanligtvis tredjepartsbibliotek på grund av `FileManager`-klassens robusthet. Dock, för mer komplex filhantering och kontroll, erbjuder bibliotek som **Files** av John Sundell ett mer Swift-vänligt API.

Så här kan du använda det:

Först, lägg till Files i ditt projekt via Swift Package Manager.

Sedan kan du kontrollera om en katalog finns på följande sätt:

```swift
import Files

do {
    _ = try Folder(path: "/path/to/your/directory")
    print("Katalogen finns")
} catch {
    print("Katalogen finns inte")
}
```

Notera: Eftersom tredjepartsbibliotek kan ändra sig, referera alltid till den senaste dokumentationen för användning och bästa praxis.
