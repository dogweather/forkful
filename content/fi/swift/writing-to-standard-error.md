---
title:                "Kirjoittaminen vakiovirheeseen"
date:                  2024-01-19
simple_title:         "Kirjoittaminen vakiovirheeseen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standard error (stderr) on erillinen tiedonvirta tavallisesta tulostusvirrasta (stdout). Virheiden, varoitusten ja diagnostisten viestien kirjoittaminen stderr:iin auttaa erottamaan ne normaalista ohjelman tulosteesta.

## How to:
Kirjoita stderr:iin käyttämällä `FileHandle.standardError`.

```Swift
import Foundation

// Viesti, joka kirjoitetaan stderr:iin
let errorMessage = "Tapahtui odottamaton virhe.\n"

// Muunnetaan viesti dataksi
if let data = errorMessage.data(using: .utf8) {
    // Kirjoitetaan data stderr:iin
    FileHandle.standardError.write(data)
}
```

Kun ajat tämän koodin, näet virheilmoituksen terminaalissa.

## Deep Dive
Ennen Swift 3:n vaikutusta, stderr:iin kirjoittaminen edellytti C:n standardikirjaston `fprintf` funktiota. Nyt Swift tarjoaa paremman abstraktion `FileHandle`-olion muodossa. Vaihtoehtoisesti voit ohjata `stderr` virtaa ohjelmasi ulkopuolelle käyttämällä komentorivityökaluja, kuten `2>`. Implementation details include how `FileHandle` works with file descriptors provided by the operating system.

## See Also
Lue lisää aiheista, kuten virranhallinta ja `FileHandle`, Swiftin virallisesta dokumentaatiosta:

- Apple's Standard Library: https://developer.apple.com/documentation/swift/filehandle
- Error Handling Guide: https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html
- Advanced UNIX Programming: https://www.bell-labs.com/usr/dmr/www/apbook.pdf (Unix-virtojen syvämmälle ymmärtämiselle)
