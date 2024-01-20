---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Gleam: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tarkistetaan, onko hakemisto olemassa selventääksemme, löytyykö hakemisto filejärjestelmästämme. Ohjelmoijat tekevät tämän virheiden välttämiseksi, jos he yrittävät lukea tai kirjoittaa hakemistoon, jota ei ole olemassa.

## Miten tehdä:

```Gleam
import gleam/filesystem.{File}

let file_handle = File.open("path/to/dir", [])
IO.puts(File.exists?(file_handle)
```
Jos yllä oleva koodiajona ajetaan ja hakemisto on olemassa, se tulostaa `True` konsoliin. Ellei, se palauttaa `False`.

## Syvällinen sukellus:

Tarkistamalla, onko hakemisto olemassa, on suhtellisen uusi käsite, kun tarkastellaan ohjelmoinnin pitkää historiaa. Vain viime vuosikymmenillä, kun tiedostojärjestelmät ovat kasvaneet massiivisesti, tällainen toiminnallisuus on tullut välttämättömäksi.

Vaihtoehtoisena lähestymistapana, jotkut ohjelmoijat saattavat yrittää avata hakemistoa ja nähdä, palautetaanko virhe. Tämä on kuitenkin harvinaista ja saattaa johtaa hämmentäviin virheviesteihin.

Gleam-kielessä, hakemiston olemassaolon tarkistaminen tehdään `filesystem`-moduulissa, esimerkiksi käyttämällä `File.exists?` -funktiota. Tämä funktio avaa sille syötetyn filePath merkkijonon ja palauttaa arvon `True`, jos hakemisto on olemassa. Muussa tapauksesssa se palauttaa `False`.

## Katso myös:

- [Gleam Filesystem documentation](https://hexdocs.pm/gleam_stdlib/gleam/filesystem/)
- [Understanding file systems](https://developer.mozilla.org/en-US/docs/Web/API/File_and_Directory_Entries_API)
- [Gleam introduction](https://gleam.run/introduction/)