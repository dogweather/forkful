---
title:    "C: Merkkijonon pituuden laskeminen"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit löytää merkkijonon pituuden? Monet C-ohjelmoijat kohtaavat tämän tarpeen työskennellessään merkkijonojen kanssa, joten on tärkeää ymmärtää, miten tämä tehdään.

## Miten

On olemassa useita tapoja löytää merkkijonon pituus C-ohjelmassa, mutta yleisin ja helpoin tapa on käyttää sisäänrakennettua `strlen()` -funktiota. Tämä funktio hyödyntää `<string.h>` -kirjastoa ja palauttaa merkkijonon pituuden kokonaislukuna.

```C
#include <string.h>
#include <stdio.h>

int main(void) {
    char string[10] = "Hei!";
    int length = strlen(string);
    printf("Merkkijonon pituus on %d\n", length);
    return 0;
}

/* Tulostaa:
Merkkijonon pituus on 4
*/
```

## Syvällisempi tarkastelu

Kuten näemme esimerkissä, `strlen()` -funktio vetää parametrinaan merkkijonon ja palauttaa sen pituuden kokonaislukuna. Mutta mitä tämä funktio tekee taustalla?

C-kielen merkkijonot lopetetaan aina null-terminaattorilla eli `'\0'`-merkillä. `strlen()` -funktio lukee merkkijonon jokaisen merkin, kunnes se saavuttaa null-terminaattorin. Se laskee tällöin kulkemansa merkkien määrän ja palauttaa sen.

On myös tärkeä huomata, että `strlen()` ei laske mukaan null-terminaattoria merkkijonon pituuteen. Tämä johtuu siitä, että `'\0'` ei ole merkkijonon osa, vaan se ilmaisee ainoastaan merkkijonon loppumisen.

## Katso myös

- [C-kielinen merkkijono-opas](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Virallinen C-kielen dokumentaatio](https://devdocs.io/c/string/string-handling)
- [Muita hyödyllisiä C-tutoriaaleja ja resursseja](https://www.codementor.io/c/tutorials)

## Katso myös

Alkuperäinen versio englanniksi: https://www.example.com/c-finding-string-length