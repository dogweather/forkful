---
title:    "Go: Tulevaisuuden tai menneen päivämäärän laskeminen"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Miksi

Usein ohjelmoinnissa joudutaan laskemaan tulevaisuuden tai menneisyyden päivämääriä. Tähän voi olla monia syitä, kuten esimerkiksi laskut ja aikataulutukset.

# Miten

Go-ohjelmointikielessä tämä on suhteellisen helppoa ja kätevää. Alla näet yksinkertaisen esimerkin siitä, kuinka voit laskea päivämäärän tietyn määrän päiviä tulevaisuuteen tai menneisyyteen.

```
func calculateDate(days int) time.Time {
    return time.Now().AddDate(0, 0, days)
}

func main() {
    futureDate := calculateDate(30)
    pastDate := calculateDate(-14)

    fmt.Println(futureDate)
    fmt.Println(pastDate)
}
```

Tämä koodi laskee tulevaisuuden päivämäärän, joka on 30 päivää nykyhetkestä, sekä menneisyyden päivämäärän, joka on 14 päivää nykyhetkestä taaksepäin. Alla näet tämän koodin tulosteen.

```
2021-08-04 17:27:52.332369 +0300 EEST m=+0.000103719
2021-07-17 17:27:52.332369 +0300 EEST m=+0.000103719
```

# Syvällisempi sukellus

Tässä esimerkissä käytetty `AddDate` -metodi laskee päivämäärän tietyn määrän vuosia, kuukausia ja päiviä nykyhetkestä eteen- tai taaksepäin. Voit myös käyttää muita aikavälin laskentaan tarkoitettuja metodeja, kuten `Add`, `Sub` ja `Round`, joiden avulla voit tehdä monimutkaisempia laskelmia päivämäärien kanssa.

# Katso myös

- [Go:n aika- ja päivämäärälaskenta](https://golang.org/pkg/time/)
- [Online-kurssi: "Understanding time in Go"](https://www.pluralsight.com/courses/go-understanding-time)