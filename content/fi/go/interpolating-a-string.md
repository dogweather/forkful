---
title:                "Nykyjonojen interpolointi"
html_title:           "Go: Nykyjonojen interpolointi"
simple_title:         "Nykyjonojen interpolointi"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Stringien interpolointi on tekniikka, joka mahdollistaa muuttujien lisäämisen merkkijonoihin. Tämä tekee koodista dynaamisempaa, sillä muuttujia voidaan käyttää joustavasti erilaisten lauseiden ja lausekkeiden muodostamiseen. 

Stringien interpolointi on kätevä tapa lisätä monimuotoisuutta koodiin ja tehdä koodista helpommin luettavaa ja ymmärrettävää. 

## Miten:
Stringien interpolointi suoritetaan lisäämällä muuttuja merkkijonoon sekä sijoittamalla merkkijono lainausmerkkien väliin. Tämä tehdään käyttämällä kirjoitusmerkkiä ``` ``` Go kielellä. Alla on esimerkki:
```
Go
x := 25
fmt.Println(``Merkkijono, johon on lisätty x-muuttuja:``, x)
```
Tuloste: `Merkkijono, johon on lisätty x-muuttuja: 25`

## Syvä sukellus:
Stringien interpolointi on kehittynyt merkkijonojen käsittelyn avulla. Ennen interpolointia käytettiin usein String Formatting-tekniikkaa tai konkatenointia muuttujien lisäämiseen merkkijonoihin. Nyt interpolointi on yleisesti käytössä monissa ohjelmointikielissä, kuten Go:ssa. 

On myös olemassa muita tapoja lisätä muuttujia merkkijonoihin, kuten Substitution ja f-string, mutta nämä ovat enemmän Python-kieleen liittyviä tekniikoita. 

Go:ssa stringien interpolointi tapahtuu käyttämällä ```fmt.Printf()``` ja ```fmt.Sprintf()``` -funktioita, jotka ovat osa Go:n standardikirjastoa. Näiden funktioiden avulla voidaan hallita muuttujien lisäämistä ja muotoilua merkkijonoihin. 

## Katso myös:
- [Golang.org: Stringien interpolointi](https://golang.org/doc/effective_go.html#string_interpolation)
- [Slant.co: Mikä on paras tapa interpoloida stringeja](https://www.slant.co/topics/2314/~best-way-to-interpolate-strings)