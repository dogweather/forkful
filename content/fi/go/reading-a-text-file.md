---
title:                "Go: Lukeminen tekstitiedostosta"
simple_title:         "Lukeminen tekstitiedostosta"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi lukea tekstitiedostoja Go-ohjelmoinnin parissa? Yksinkertaisesti sanoen, tiedostojen käsittely on tärkeä osa useimpien ohjelmien toimintaa. Tekstitiedostot ovat yleisimpiä tietotyyppejä, joista ohjelmat lukevat ja kirjoittavat tietoa. Siksi on tärkeää ymmärtää, miten Go-kielen avulla voi käsitellä näitä tiedostoja.

## Kuinka

Go-kielellä on monia mahdollisuuksia lukea ja käsitellä tekstitiedostoja. Yksi yksinkertainen tapa on käyttää `bufio`-pakettia. Alla on esimerkki, miten voit lukea tekstitiedoston rivi riviltä ja tulostaa sen sisällön:

```Go
file, err := os.Open("tekstitiedosto.txt") // avaa tiedosto
if err != nil { // tarkistetaan onko virheitä
    log.Fatal(err)
}

scanner := bufio.NewScanner(file) // luodaan skanneri tiedoston lukemista varten
for scanner.Scan() { 
    fmt.Println(scanner.Text()) // tulostetaan jokainen rivi
}

if err := scanner.Err(); err != nil { // tarkistetaan mahdolliset skannerivirheet
    log.Fatal(err)
}

```

Yllä oleva esimerkki käyttää `os`-pakettia tiedoston avaamiseen, `bufio`-pakettia tiedoston lukemista varten ja `fmt`-pakettia tulostamiseen. Voit myös käyttää `io/ioutil`-pakettia, joka tarjoaa yksinkertaisemman tavan lukea tekstitiedostoja.

## Syväsukellus

Tiedostojen lukeminen ja käsittely voi olla monimutkaisempaa kuin yllä olevassa esimerkissä. Esimerkiksi jos haluat lukea vain tietyn tiedoston osan, sinun täytyy käyttää `io.Seek()`-funktiota. Tiedostojen lukeminen voi myös suorittaa monia muita toimintoja, kuten muuttaa tiedostojen nimiä, päivittää niitä tai jopa luoda niitä. Siksi on tärkeää tutkia Go-kielellä olevia erilaisia työkaluja ja paketteja, jotka voivat auttaa sinua käsittelemään tekstitiedostoja tehokkaasti.

## Katso myös

- https://gobyexample.com/reading-files - Kattava esimerkki tutustuttaa lukemiseen ja kirjoittamiseen tekstitiedostoissa Go-kielellä.
- https://golang.org/pkg - Virallinen Go-kielen dokumentaatio, joka sisältää kattavan listan paketteja ja toimintoja tiedostonkäsittelyyn.
- https://github.com/go-kratos/kratos/tree/master/pkg/os - Esimerkkejä siitä, miten joissakin avoimen lähdekoodin projekteissa käsitellään tiedostoja käyttäen Go-kieltä.