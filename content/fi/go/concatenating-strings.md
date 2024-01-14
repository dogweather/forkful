---
title:                "Go: Merkkijonojen yhdistäminen"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Jokaisessa ohjelmointikielessä on omat tärkeät toiminnot, joita tarvitaan jokaisessa ohjelmassa. Yksi tällainen toiminto on merkkijonojen yhdistäminen. Se on tärkeä, koska se mahdollistaa useiden merkkijonojen yhdistämisen yhteen merkkijonoon.

## Kuinka tehdä 

Merkkijonojen yhdistäminen Go-kielellä on helppoa ja yksinkertaista. Se voidaan tehdä käyttämällä sisäänrakennettua `+`-operaattoria. Alla on esimerkki koodista, joka yhdistää kaksi merkkijonoa (`Hello` ja `world`) yhteen merkkijonoon (`Hello world`).

```Go
package main

import "fmt"

func main() {
    greeting := "Hello"
    name := "world"

    fmt.Println(greeting + " " + name)
}
```

Tulostus:

`Hello world`

## Syvällinen tarkastelu

Merkkijonojen yhdistäminen ei ole vain yksinkertainen ja kätevä työkalu, vaan se mahdollistaa myös joustavuuden ja monimutkaisempien merkkijonojen luomisen. Yhdistämällä merkkijonoja voit esimerkiksi luoda muuttuvan viestin, joka sisältää vaihtoehtoisia arvoja.

```Go
package main

import "fmt"

func main() {
    name := "Sami"
    age := 25

    fmt.Println("Hei " + name + ", olet " + string(age) + "-vuotias!")
}
```

Tulostus:

`Hei Sami, olet 25-vuotias!`

## Katso myös
- [Go-kielen virallinen dokumentaatio](https://golang.org/doc/)
- [Merkkijonojen käsittely Go-kielellä](https://gobyexample.com/strings)
- [Go-kurssit ja opetusmateriaalit](https://blog.gobridge.org/resources-for-learning-go/)