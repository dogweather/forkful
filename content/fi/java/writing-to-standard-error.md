---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "Java: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Miksi
## Miksi kirjoittaa standardi virheeseen?

Kirjoittaminen standardi virheeseen on tärkeää, koska se auttaa kehittäjiä havaitsemaan ja korjaamaan virheitä ohjelmoinnin aikana. Kun ohjelma suoritetaan, virheet voivat ilmetä ja näkyvät tietokoneen konsolissa. Kirjoittaminen standardi virheeseen tarjoaa kehittäjille tärkeää tietoa virheistä ja auttaa heitä ymmärtämään, mitä osaa koodista tulee tarkastella ja korjata.

# Miten
## Miten kirjoittaa standardi virheeseen?

Käytä Java-kielen _System.err_ -luokkaa kirjoittaaksesi standardi virheeseen koodisi sisällä seuraavalla tavalla:

```
Java System.err.println("Tämä on virheilmoitus");
```

Tämän jälkeen voit suorittaa ohjelman ja se tulostaa virheilmoituksen konsoliin.

# Syvenny
## Syvenny kirjoittamiseen standardi virheeseen

Kirjoittaessa standardi virheeseen, on tärkeää käyttää _try-catch_ -lausekkeita virheiden käsittelyyn ja hallintaan. _System.err_ -luokka tarjoaa myös muita hyödyllisiä toimintoja, kuten _printStackTrace()_, joka auttaa selvittämään, missä kohdassa koodissa virhe on tapahtunut.

Kun käytät virheen ilmaisua _System.err_, on tärkeää myös muistaa koodin luettavuus ja käytettävyys. On hyvä idea määritellä oma luokka virheiden hallintaa varten, joka voi sisältää erilaisia virheen käsittelymetodeja riippuen ohjelmasta ja sen tarkoituksesta.

# Katso myös
- [Java System.err Javadoc](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#err)
- [Java Exception handling tutorial](https://www.javatpoint.com/exception-handling-in-java)
- [Good coding practices in Java](https://www.vogella.com/tutorials/JavaBestPractices/article.html)