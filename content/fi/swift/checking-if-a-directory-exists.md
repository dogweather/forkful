---
title:                "Tarkastetaan löytyykö kansio"
html_title:           "Swift: Tarkastetaan löytyykö kansio"
simple_title:         "Tarkastetaan löytyykö kansio"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Miksi, miksi ohjelmoijat tarkistavat, onko hakemisto olemassa, ja mitä se tarkoittaa.

Hakemisto on tietokoneen osa, joka sisältää useita tiedostoja tai muita hakemistoja. Kun ohjelmoijat tarkistavat, onko hakemisto olemassa, he haluavat varmistaa, että se on saatavilla käytettäväksi ohjelmassa. Tämä auttaa estämään virheilmoituksia ja mahdollistaa sujuvamman koodin suorituksen.

Miten se tehdään:

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users" // vaihda tämä haluamaasi hakemistoon

// Tarkistetaan, onko hakemisto olemassa
if fileManager.fileExists(atPath: directoryPath) {
    // Jos hakemisto on olemassa, tulostetaan tiedot
    if let directoryContents = try? fileManager.contentsOfDirectory(atPath: directoryPath) {
        print("Hakemistossa on seuraavat tiedostot: \(directoryContents)")
    }
} else {
    print("Hakemistoa ei löytynyt")
}
```

Tässä ohjeessa käytämme FileManager-luokkaa, joka on saatu Swift Foundation -kehyksestä. Ensimmäisellä rivillä määritämme FileManager-olion ja toisella rivillä asetamme halutun hakemistosijainnin muuttujaan "directoryPath". Seuraavaksi tarkistamme, onko hakemisto olemassa käyttämällä "fileExists"-metodia. Jos se on olemassa, tulostamme hakemistossa olevien tiedostojen nimet. Muussa tapauksessa tulostamme, että hakemistoa ei löytynyt.

Syvemmälle mennään:

Historiallinen tausta: Hakemistojen olemassaolon tarkistaminen on ollut osa ohjelmointia jo pitkään. Aiemmin se tehtiin käyttämällä erilaisia järjestelmäkutsuja tai Shell-komentoja. Swiftin FileManager-luokka tuo tämän toiminnallisuuden kätevästi kehykseen, mikä helpottaa kehittäjän työtä.

Vaihtoehtoiset vaihtoehdot: Hakemistojen olemassaolon tarkistamiseen on muitakin tapoja, kuten käyttämällä Foundationin "URL"-luokkaa tai Unix Shell-komentoja. Jokaisella näistä tavoista on omat etunsa ja haittansa, joten on hyvä tuntea eri vaihtoehtoja ja valita niistä sopivin tapauskohtaisesti.

Tarkemmat tiedot: Tarkistaaksesi oikeassa hakemistossa käytetyt järjestelmäkutsut ja komentojen syntaksi, voit tutustua viralliseen dokumentaatioon osoitteessa https://developer.apple.com/documentation/foundation/filemanager ja https://developer.apple.com/legacy/library/documentation/Darwin/Reference/ManPages/man1/find.1.html.

Katso myös: Jos haluat lukea lisää FileManager-luokasta ja sen tarjoamista toiminnoista, voit tutustua myös seuraaviin lähteisiin:
- https://www.hackingwithswift.com/read/3/5/working-with-directories-in-swift
- https://developer.apple.com/documentation/foundation/filemanager/1408285-fileexists