---
title:                "Koodin järjestäminen funktioihin"
date:                  2024-01-26T01:11:15.288473-07:00
model:                 gpt-4-1106-preview
simple_title:         "Koodin järjestäminen funktioihin"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

# Mikä & Miksi?
Koodin järjestäminen funktioihin on koodin pilkkomista uudelleenkäytettäviksi osiksi. Se tekee koodistasi selkeämmän, helpommin luettavan ja yksinkertaisemman virheenjäljityksen kannalta.

# Miten:
Tässä Go-koodinpätkä, joka näyttää koodilohkon, minkä jälkeen uudelleenjärjestetty versio funktioita käyttäen:

```go
package main

import "fmt"

func main() {
    // Ennen: Inline-koodi
    fmt.Println("Lasketaan summaa...")
    yhteensa := 0
    for i := 1; i <= 10; i++ {
        yhteensa += i
    }
    fmt.Println("Kokonaissumma on:", yhteensa)

    // Jälkeen: Funktion käyttäminen
    fmt.Println("Lasketaan summaa funktion avulla...")
    summa := getSum(1, 10)
    fmt.Println("Kokonaissumma on:", summa)
}

// Funktio summan laskemiseksi tietyllä välimatkalla
func getSum(alku, loppu int) int {
    yhteensa := 0
    for i := alku; i <= loppu; i++ {
        yhteensa += i
    }
    return yhteensa
}
```

Molempien, inline-koodin sekä funktion perusteella toteutetun koodin tuloste on sama:

```
Lasketaan summaa...
Kokonaissumma on: 55
Lasketaan summaa funktion avulla...
Kokonaissumma on: 55
```

# Syväsukellus
Ennen funktioiden käsitteen esiintuloa ohjelmointi oli pääasiassa proseduraalista, ja koodi suoritettiin ylhäältä alas. Ohjelmien kasvaessa tämä lähestymistapa aiheutti tehotonta toimintaa ja koodin toistoa.

Kielet esittelivät funktiot abstraktiomekanismina. Go-kielessä funktiot kapseloivat koodilohkoja, joilla on tietty tehtävä, ja kannustavat DRY (Don't Repeat Yourself) -periaatteen noudattamiseen. Ne hyväksyvät parametrejä ja voivat palauttaa tuloksia.

Hyödyllisiä vinkkejä:
- Nimeä funktiot selkeästi; hyvä nimi kertoo mitä funktio tekee.
- Pidä ne lyhyinä; jos funktio tekee liian paljon, pilko se osiin.
- Funktiot voivat palauttaa useita arvoja, hyödynnä tätä virheenkäsittelyssä.
- Korkeamman asteen funktiot (funktiot, jotka ottavat vastaan tai palauttavat muita funktioita) ovat voimakkaita työkaluja Go-kielessä.

Vaihtoehtoja funktioille ovat inline-koodi (sekavaa monimutkaisissa tehtävissä) ja oliomenetelmät (oliotyyppisen paradigman osa, joka on Go:ssa saatavilla rakenteiden kautta).

# Katso Myös
- [Go by Example: Funktionaalinen ohjelmointi](https://gobyexample.com/functions)
- [Tehokas Go: Funktionaalinen ohjelmointi](https://golang.org/doc/effective_go#functions)
