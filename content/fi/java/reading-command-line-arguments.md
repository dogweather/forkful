---
title:                "Java: Komentoriviparametrien lukeminen"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi
On monia tilanteita, joissa ohjelmoijien täytyy lukea komentoriviparametreja ohjelmien suorituksen aikana. Tämä artikkeli tarjoaa ymmärrettävän selityksen ja selkeät esimerkit siitä, miksi ja miten komentoriviparametrit luetaan Java-ohjelmoinnissa.

## Miten
Komentoriviparametrien lukeminen Java-ohjelmassa on helppoa. Aluksi tarvitsemme `java.lang.String[]`-taulukon, johon tallennamme parametrit. Parametrit lähetetään `main`-metodille merkkijonotaulukkona `args`-parametrilla. Tämän jälkeen voimme käyttää `for`-silmukkaa käsittelemään kaikki parametrit yksitellen. Katso esimerkki:

```Java
public static void main(String[] args){
  for (int i = 0; i < args.length; i++){
    System.out.println("Parametri " + i + ": " + args[i]);
  }
}
```

Jos haluamme antaa ohjelmalle parametreja, voimme lisätä niitä komentoriville, esimerkiksi `java Ohjelma parametri1 parametri2`. Kun suoritamme tämän, tulostuu seuraava viesti:

```
Parametri 0: parametri1
Parametri 1: parametri2
```

## Syvällinen sukellus
Komentoriviparametrien lukeminen on erittäin hyödyllinen ominaisuus ohjelmoinnissa, sillä se mahdollistaa käyttäjän syöttämien tietojen käsittelyn ja hyödyntämisen ohjelman toiminnassa. `String[]`-taulukon avulla voimme helposti käsitellä ja muokata parametreja haluamallamme tavalla. Voimme myös käyttää `if`-lauseita tai `switch`-lauseita tunnistamaan ja toimimaan eri tavalla eri parametrien arvojen perusteella.

## Katso myös
- Java-ohjelmointikie