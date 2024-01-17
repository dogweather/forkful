---
title:                "Lähetetään http-pyyntö"
html_title:           "Go: Lähetetään http-pyyntö"
simple_title:         "Lähetetään http-pyyntö"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
HTTP-pyynnön lähettäminen on tapa kommunikoida tietokoneen ja verkkopalvelimen välillä. Se on tärkeää ohjelmoijille, koska se mahdollistaa tiedon ja resurssien hakemisen Internetistä, mikä on hyödyllistä monenlaisten sovellusten luomisessa.

## Miten:
```Go
resp, err := http.Get("https://example.com/")
if err != nil {
	fmt.Println("Virhe HTTP-pyynnön lähettämisessä:", err)
} else {
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Println("Failed to read response body:", err)
	} else {
		fmt.Println(string(body))
	}
}
```

```Go
resp, err := http.Post("https://example.com/", "application/json", bytes.NewBuffer(jsonStr))
if err != nil {
	fmt.Println("Virhe HTTP-pyynnön lähettämisessä:", err)
} else {
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Println("Failed to read response body:", err)
	} else {
		fmt.Println(string(body))
	}
}
```

## Syventävä sukellus:
HTTP-protokolla kehitettiin 1990-luvun alussa mahdollistamaan sivujen ja muiden resurssien jakaminen verkon kautta.

Go-kielellä on myös muita tapoja lähettää HTTP-pyyntöjä, kuten `http.Head()` ja `http.Do()`. On myös mahdollista muokata otsakoita ja lisätä muita parametreja pyyntöihin.

HTTP-pyynnön lähetyksessä on monia eri osia, kuten URL, otsakkeet, parametrit ja sisältö. Go tarjoaa helpon tavan käsitellä näitä osia ja lähettää pyyntöjä tehokkaasti.

## Katso myös:
- [The Go Programming Language](https://golang.org/)
- [HTTP Client - Go Doc](https://golang.org/pkg/net/http/#Client)
- [HTTP - Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/HTTP)