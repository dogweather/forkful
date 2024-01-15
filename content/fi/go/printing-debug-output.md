---
title:                "Tulostaminen vianetsintätilanteessa"
html_title:           "Go: Tulostaminen vianetsintätilanteessa"
simple_title:         "Tulostaminen vianetsintätilanteessa"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi
Jokaisella kehittäjällä tulee vastaan tilanteita, jolloin koodia täytyy debugata. Tässä tilanteessa debug-otulosteen tulostaminen voi olla äärimmäisen hyödyllistä, sillä se auttaa hahmottamaan mikä koodissa ei toimi halutulla tavalla.

## Miten tehdä
Debug-otulosteen tulostaminen Go:lla on helppoa. Käytetään ```fmt.Println()```-funktiota ja siihen lisätään haluttu debug-viesti. Alla olevassa esimerkissä tulostetaan debug-viesti "Hei maailma!".

```Go
fmt.Println("Hei maailma!")
```

Tämä tulostaisi debug-viestin terminaaliin:

```
Hei maailma!
```

## Syvempi sukellus
```fmt.Println()```-funktio ei ole ainoa tapa tulostaa debug-viestejä Go:lla. Voit myös käyttää ```fmt.Printf()```-funktiota, joka antaa mahdollisuuden muotoilla tulostettavaa viestiä. Esimerkiksi voit tulostaa muuttujan arvon seuraavasti:

```Go
nimi := "Maija"
fmt.Printf("Hei %s, tervetuloa!", nimi)
```

Tulostettu tulos olisi:

```
Hei Maija, tervetuloa!
```

Syvemmälle debuggaamisen maailmaan sukeltamiseksi kannattaa tutustua Go:n sisäänrakennettuun ```log```-pakettiin, joka tarjoaa monipuolisempia vaihtoehtoja debug-viestien tulostamiseen.

## Katso myös
- [Go:n virallinen dokumentaatio](https://golang.org/doc/) tarjoaa kattavan katsauksen debuggaamisen mahdollisuuksiin
- [Go:n debuggaus oppimateriaali](https://golang.org/doc/gdb) sisältää hyödyllistä tietoa Go-koodin debuggaamisesta käyttäen GNU Debugger (GDB) -työkalua.