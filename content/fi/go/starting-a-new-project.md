---
title:    "Go: Aloittaminen: Uuden projektin aloittaminen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Miksi aloittaa uusi projekti Go-kielellä? Go on moderni, tehokas ja helppokäyttöinen ohjelmointikieli, joka on erityisen hyvä ratkaisu suurten verkkopalveluiden kehittämiseen. Se tarjoaa myös monia hyödyllisiä työkaluja ja kirjastoja, jotka helpottavat ohjelmien rakentamista.

## Miten

Aloittaminen Go:n kanssa on helppoa. Aloita luomalla uusi kansio ja avaa se terminaalissa. Kirjoita ensimmäiseksi `go mod init projektin_nimi`, jolloin Go luo projektillesi moduulin hallinnoimista varten. Seuraavaksi voit luoda uuden tiedoston `main.go` ja aloittaa kirjoittamaan koodia.

```
Go import (
  "fmt"
)

func main() {
  fmt.Println("Tervetuloa Go:n maailmaan!")
}
```

Tämän jälkeen voit suorittaa ohjelman komennolla `go run main.go` ja näet tulosteen terminaalissa. Näin olet luonut ensimmäisen Go-ohjelmasi!

## Syvemmälle

Go-kielellä on monia työkaluja ja kirjastoja, jotka auttavat kehittämään tehokkaita ja luotettavia ohjelmia. Suosittelemme tutustumaan seuraaviin resursseihin:

- [Go:n virallinen dokumentaatio](https://golang.org/doc/)
- [Awesome Go -listaus erilaisista Go-projekteista](https://github.com/avelino/awesome-go)
- [Go:n oppimateriaalit ja esimerkkikoodit](https://github.com/golang/go/wiki/Learn%20to%20Program)
- [Visual Studio Code ja sen Go-laajennus](https://code.visualstudio.com/docs/languages/go)

Muista myös hyödyntää Go-koodisi versionhallintaa, kuten Git, ja jakaa projektejasi muiden kanssa esimerkiksi GitHubissa.

## Katso myös

- [Go:n viralliset sivut](https://golang.org/)
- [Go-kielestä suomeksi](https://github.com/melix/golang-suomi)
- [Go:n käyttöohjeet ja tutoriaalit](https://golang.org/doc/tutorial/)