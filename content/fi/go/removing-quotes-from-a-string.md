---
title:                "Merkkijonosta lainausmerkkien poistaminen"
date:                  2024-01-26T03:39:32.588595-07:00
model:                 gpt-4-0125-preview
simple_title:         "Merkkijonosta lainausmerkkien poistaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lainausmerkkien poistaminen merkkijonosta tarkoittaa kyseisten kiusallisten kaksois- tai yksittäisten lainausmerkkien hävittämistä tekstisi ympäriltä. Tämän teemme puhdistaaksemme dataa, estääksemme jäsentelyvirheitä tai valmistellaksemme tekstiä jatkokäsittelyyn ilman lainausmerkkien lisäkuormaa.

## Kuinka:

Tässä on yksinkertainen tapa potkaista nuo lainausmerkit syrjään Go:ssa:

```go
package main

import (
	"fmt"
	"strings"
)

func removeQuotes(s string) string {
	return strings.Trim(s, "'\"")
}

func main() {
	quotedString := "\"Hei, Maailma!\""
	fmt.Println("Alkuperäinen:", quotedString)

	unquotedString := removeQuotes(quotedString)
	fmt.Println("Lainausmerkitön:", unquotedString)
}
```

Tuloste näyttää tältä, lainausmerkit poissa:

```
Alkuperäinen: "Hei, Maailma!"
Lainausmerkitön: Hei, Maailma!
```

## Syväsukellus

Takaisin siihen aikaan, kun datamuodot ja tiedonvaihto eivät olleet standardoituja, lainausmerkit merkkijonoissa saattoivat aiheuttaa kaaosta. Ne voivat edelleen, erityisesti JSON:ssa tai kun työnnät merkkijonoja tietokantoihin. Go:n `strings`-paketti sisältää `Trim`-funktion, joka ei ainoastaan poista välilyöntejä, vaan myös mitä tahansa merkkejä, joista et pidä.

Miksi ei sitten Regex? No, `Trim` on nopeampi yksinkertaisissa tehtävissä, mutta jos merkkijonosi piilottelevat lainausmerkkejä kummallisissa paikoissa, regex saattaa olla sinun raskas tykistösi:

```go
import "regexp"

func removeQuotesWithRegex(s string) string {
	re := regexp.MustCompile(`^["']|["']$`)
	return re.ReplaceAllString(s, "")
}
```

Se on kuin valitsisi sakset tai moottorisahan; valitse työkalu sopivaan tehtävään.

## Katso myös

Lisää `strings`-paketista ja sen voimatyökaluista:
- [Paketti strings](https://pkg.go.dev/strings)

Käyttääksesi säännöllisten lausekkeiden voimaa Go:ssa:
- [Paketti regexp](https://pkg.go.dev/regexp)

Haluatko sukeltaa syvemmälle merkkijonojen trimmauksen filosofiaan?
- [Trim-metodi](https://blog.golang.org/strings)