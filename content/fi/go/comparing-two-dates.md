---
title:    "Go: Kahden päivämäärän vertailu"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Miksi vertailla kahden päivämäärän välillä?

Kun käsittelemme päivämääriä Go-ohjelmoinnin kanssa, on tärkeää ymmärtää, kuinka vertailla kahta päivämäärää toisiinsa. Tämä mahdollistaa esimerkiksi tapahtuman päivämäärän tarkistamisen tai lähimmän tulevan päivämäärän laskemisen.

## Kuinka tehdä se?

Voit vertailla kahta päivämäärää Go-ohjelmoinnissa käyttämällä ```time```-pakettia ja sen ```Before```, ```After``` tai ```Equal```-metodeja. Seuraavassa esimerkissä tarkistamme, onko ensimmäinen päivämäärä ennen tai jälkeen toisen päivämäärän:

```
t1 := time.Date(2021, time.March, 1, 0, 0, 0, 0, time.UTC)
t2 := time.Date(2021, time.April, 1, 0, 0, 0, 0, time.UTC)
if t1.Before(t2) {
    fmt.Println("Ensimmäinen päivämäärä on ennen toista päivämäärää.")
} else if t1.After(t2) {
    fmt.Println("Ensimmäinen päivämäärä on jälkeen toista päivämäärää.")
} else {
    fmt.Println("Päivämäärät ovat samat.")
}
```

**Tuloste:**
```
Ensimmäinen päivämäärä on ennen toista päivämäärää.
```

## Syvenny aiheeseen

Päivämäärän vertailu Go-ohjelmoinnissa käyttää taustalla Unix-ajanlaskua. Jokainen päivämäärä edustaa tiettyä sekuntia Unix-ajanlaskusta. Tämän vuoksi voimme käyttää myös Unix-aikaleimoja päivämäärien vertailuun. Seuraavassa esimerkissä tarkistamme, onko ensimmäinen päivämäärä ennen tai jälkeen toisen päivämäärän Unix-aikaleimojen avulla:

```
t1Unix := time.Date(2021, time.March, 1, 0, 0, 0, 0, time.UTC).Unix()
t2Unix := time.Date(2021, time.April, 1, 0, 0, 0, 0, time.UTC).Unix()
if t1Unix < t2Unix {
    fmt.Println("Ensimmäinen päivämäärä on ennen toista päivämäärää.")
} else if t1Unix > t2Unix {
    fmt.Println("Ensimmäinen päivämäärä on jälkeen toista päivämäärää.")
} else {
    fmt.Println("Päivämäärät ovat samat.")
}
```

**Tuloste:**
```
Ensimmäinen päivämäärä on ennen toista päivämäärää.
```

# Katso myös

- [Go time-paketti](https://golang.org/pkg/time/)
- [Go Unix ajanlasku](https://golang.org/pkg/time/#Time.Unix)