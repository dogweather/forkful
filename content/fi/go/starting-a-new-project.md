---
title:                "Aloittamassa uutta projektia"
html_title:           "Go: Aloittamassa uutta projektia"
simple_title:         "Aloittamassa uutta projektia"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

Mitä & Miksi?:
Aloittavan uuden projektin luominen on prosessi, jossa ohjelmoija luo uuden ohjelmiston tai sovelluksen. Tämä voi olla tarpeen esimerkiksi silloin, kun ohjelmaa tarvitaan uuden ongelman ratkaisemiseen tai vanhan parantamiseen.

Miten:
Esimerkiksi uuden projektin luomiseen Go-kielellä tarvitaan vain muutamia yksinkertaisia askelia. Ensimmäiseksi tulee määrittää tarvittavat paketit käyttämällä "import" -komentoa. Sitten voidaan luoda uusi tiedosto käyttäen "main" -funktiota. Lopuksi ohjelmaa voidaan suorittaa käyttämällä "go run" -komentoa. Tässä on esimerkki:

```Go
import "fmt"

func main() {
    fmt.Println("Hei maailma!")
}
```
Tämä tulostaa "Hei maailma!" konsoliin.

Syväsukellus:
Go-kieli kehitettiin vuonna 2009 Googlella ja se on nopeasti kasvattanut suosiotaan ohjelmointikielenä. Sen tarkoituksena on olla tehokas ja helppokäyttöinen vaihtoehto esimerkiksi C-kielen tilalle. Muita suosittuja vaihtoehtoja uuden projektin aloittamiseen ovat esimerkiksi Python, Java ja C++.

Katso myös:
Lisätietoja Go-kielestä löytyy viralliselta verkkosivustolta osoitteessa https://golang.org. Sieltä löytyvät myös ohjeet asennukseen ja lisätarvikkeisiin, kuten text editor -ohjelma. Lisäksi suosittelemme tutustumaan Go-yhteisöön Githubissa https://github.com/golang/go, josta löytyy paljon hyödyllistä tietoa ja vinkkejä aloittelijoille.