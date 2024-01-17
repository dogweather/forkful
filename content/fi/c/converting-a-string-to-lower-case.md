---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "C: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Tervetuloa lukemaan C (nykyinen versio) ohjelmointiartikkelia, jossa käsitellään merkkijonon muuntamista pieniksi kirjaimiksi. Tässä artikkelissa selitetään lyhyesti, mitä tämä tarkoittaa ja miksi ohjelmoijat tekevät sitä. Artikkelin suoraviivaisessa tyylissä annetaan myös esimerkkejä koodista ja lisäinfoa aiheesta. Löydät myös linkkejä muihin asiaa liittyviin lähteisiin "See Also" osiosta.

## What & Why?

Merkkijonon muuntaminen pieniksi kirjaimiksi tarkoittaa sitä, että kaikki merkit muutetaan aakkosissa pieniksi kirjaimiksi. Ohjelmoijat tekevät tätä usein silloin, kun he haluavat tarkistaa tai vertailla merkkijonoja, mutta haluavat ohittaa mahdolliset suuret ja pienet erot kirjaimissa.

## How to:

Esimerkiksi, jos käytämme seuraavaa koodia:

```C
char string[] = "KOODI ON KIVAA";
char *ptr = string;

while (*ptr)
{
    *ptr = tolower(*ptr);
    ptr++;
}

printf("%s", string);
```

Saamme seuraavan tulosteen:

```
"koodi on kivaa" 
```

Koodissa ensin määrittelemme merkkijonon ja sen jälkeen luomme osoittimen merkkijonon alkuun. Sitten käytämme while-loopia, joka käy läpi kaikki merkit merkkijonossa ja muuttaa ne pieniksi kirjaimiksi käyttäen tolower-funktiota. Lopuksi tulostamme muunnetun merkkijonon.

## Deep Dive:

Historiallisessa kontekstissa, merkkijonon muuttaminen pieniksi kirjaimiksi on tehty erilaisilla tavoilla eri ohjelmointikielillä. Esimerkiksi joissain kielissä on olemassa erikseen pienet ja suuret kirjaimet, kun taas toisissa kielissä ei ole. C kielen tolower-funktio ottaa huomioon kullakin ympäristössä käytössä olevan merkistön ja sen mukaan muuttaa merkit vastaaviksi pieniksi kirjaimiksi.

On myös olemassa muita tapoja muuttaa merkkijonoja pieniksi kirjaimiksi, kuten käyttämällä kirjastoja tai kirjoittamalla oma tolower-funktio, mutta useimmiten C kielen tolower on nopea ja kätevä vaihtoehto.

## See Also:

- C tolower man-sivu: https://linux.die.net/man/3/tolower
- C String Library: https://www.programiz.com/c-programming/library-function/string.h/tolower