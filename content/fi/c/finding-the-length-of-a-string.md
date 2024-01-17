---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "C: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Jonoja, ohjelmoijat joutuvat usein työskentelemään merkkijonojen kanssa. Merkkijono on erilaisten merkkien jono, kuten esimerkiksi "Tämä on merkkijono". Jotta voimme käsitellä merkkijonoja tehokkaasti, on tärkeää pystyä määrittämään niiden pituus. Tämä tarkoittaa, kuinka monta merkkiä tai symbolia merkkijonossa on.

## Miten:
Esimerkiksi, jos haluamme selvittää merkkijonon "Hei maailma" pituuden, voimme käyttää ```strlen```-funktiota C-ohjelmoinnissa.

```C
#include <stdio.h>
#include <string.h>

int main() {
  char merkkijono[] = "Hei maailma";
  int pituus = strlen(merkkijono);

  printf("Merkkijonon pituus on %d", pituus);
  return 0;
}
```
Tulostus on:
```
Merkkijonon pituus on 11
```

## Syvemmälle:
Historiallisesti, merkkijonojen pituuden määrittämiseen on käytetty manuaalista laskentaa, kun taas nykyään voimme käyttää valmiita funktioita kuten ```strlen```. On myös muita tapoja määrittää merkkijonon pituus, kuten käyttämällä for-silmukkaa ja laskemalla merkit manuaalisesti tai käyttämällä ```sizeof```-operaattoria.

## Katso myös:
- [Guru99 tutorial about string length in C](https://www.guru99.com/c-strings.html)
- [C String Functions](https://www.programiz.com/c-programming/string-functions)
- [How to find the length of a string in C](https://www.techiedelight.com/find-length-string-c-program/)