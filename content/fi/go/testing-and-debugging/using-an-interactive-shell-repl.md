---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:35.827365-07:00
description: "Kuinka: Vaikka Go ei sis\xE4ll\xE4k\xE4\xE4n sis\xE4\xE4nrakennettua\
  \ REPL:i\xE4, yhteis\xF6 on luonut ty\xF6kaluja kuten `gore` t\xE4ytt\xE4m\xE4\xE4\
  n t\xE4m\xE4n aukon. Asenna `gore` suorittamalla\u2026"
lastmod: '2024-03-13T22:44:56.052331-06:00'
model: gpt-4-0125-preview
summary: "Vaikka Go ei sis\xE4ll\xE4k\xE4\xE4n sis\xE4\xE4nrakennettua REPL:i\xE4\
  , yhteis\xF6 on luonut ty\xF6kaluja kuten `gore` t\xE4ytt\xE4m\xE4\xE4n t\xE4m\xE4\
  n aukon."
title: "Interaktiivisen kuoren (REPL) k\xE4ytt\xE4minen"
weight: 34
---

## Kuinka:
Vaikka Go ei sisälläkään sisäänrakennettua REPL:iä, yhteisö on luonut työkaluja kuten `gore` täyttämään tämän aukon. Asenna `gore` suorittamalla ensin:

```
$ go get -u github.com/motemen/gore
```

Kun se on asennettu, käynnistä `gore` kirjoittamalla `gore` päätteeseesi:

```
$ gore
```

Sinun pitäisi nähdä kehote, joka on valmis vastaanottamaan Go-komentoja. Kokeillaan yksinkertaista esimerkkiä:

```
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
```

Näkisit tulosteen kuten:

```
Hello, Go REPL!
```

Muuttujat ja funktiomäärittelyt toimivat odotetusti. Voit määritellä funktion:

```
gore> :import math
gore> areaCircle := func(radius float64) float64 {
...> return math.Pi * radius * radius
...> }
gore> fmt.Println("Ympyrän pinta-ala säteellä 4:", areaCircle(4))
```

Ja saat tuloksen heti:

```
Ympyrän pinta-ala säteellä 4: 50.26548245743669
```

## Syväsukellus:
REPL:n konsepti on ikivanha, jäljittäen juurensa 1960-luvun Lisp-koneisiin, tarjoten interaktiivisen ohjelmointikokemuksen. Toisin kuin kielet kuten Python tai JavaScript, Go suunniteltiin ilman REPL:iä, keskittyen sen sijaan käännetyihin binääreihin suorituskyvyn ja yksinkertaisuuden vuoksi. Tämä heijastaa Gon filosofiaa yksinkertaisuudesta ja sen suunnittelua skaalautuvaa ja ylläpidettävää ohjelmistoa varten.

Kuitenkin, työkalut kuten `gore` tai `goplay` esittelevät Go-yhteisön kekseliäisyyttä tämän aukon ylittämisessä. Nämä työkalut jäsentävät Go-koodia dynaamisesti ja käyttävät `go/eval` -pakettia tai vastaavia mekanismeja suorittaakseen sen reaaliajassa, joskin joitakin rajoituksia verrattuna natiiviin REPL-ympäristöön. Nämä rajoitukset johtuvat Gon tyyppijärjestelmästä ja käännösmallista, jotka voivat tehdä lennossa tapahtuvan arvioinnin haastavaksi.

Vaikka REPL-ympäristöt ovat erittäin hyödyllisiä koulutuksen ja nopeiden testien kannalta, Go-ekosysteemi tyypillisesti suosii perinteisiä käännä-ja-suorita -prosesseja useimmissa kehitystehtävissä. IDE:t ja editorit, joissa on Go-tuki, kuten Visual Studio Code tai GoLand, tarjoavat integroituja työkaluja testaamiseen ja vianetsintään, mikä lieventää suurelta osin tarvetta REPL:lle ammattimaisessa kehityksessä.

Tutkivalle ohjelmoinnille, prototyyppien luonnille tai oppimiselle REPL:t kuten `gore` tarjoavat kuitenkin arvokkaan vaihtoehdon, sallien ohjelmoijille, jotka ovat tottuneet REPL:iin muissa kielissä, nauttia samanlaisesta kokemuksesta Go:ssa.
