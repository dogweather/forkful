---
title:    "C: Merkkijonon alkukirjaimien suurennus"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisimme ylipäätään suorittaa merkkijonon suuraakkosiksi muuttamisen? Onko siitä hyötyä tai onko se vain tarpeetonta koodin lisäystä? Monissa tapauksissa merkkijonon suuraakkosiksi muuttaminen voi helpottaa käyttäjien syöttämien tietojen tarkistamista järjestelmässä, kun ei ole väliä, miten käyttäjä merkitsee kirjaimet. Tässä blogikirjoituksessa näytämme, miten voimme toteuttaa tämän C-ohjelmointikielellä.

## Kuinka

Yksi tapa muuttaa merkkijono suuraakkosiksi on käyttää <string.h> kirjastosta löytyvää funktiota nimeltä "toupper". Tämä funktio muuttaa annetun merkkin kohdalla olevan pienen kirjaimen vastaavaksi suureksi kirjaimeksi. Alla on yksinkertainen esimerkki, kuinka tätä funktiota voi käyttää:

```C
#include <stdio.h>
#include <string.h>

int main() {
  char s[] = "Esimerkki teksti";
  int i;

  printf("Ennen muunnosta: %s\n", s);

  for (i = 0; i < strlen(s); i++) {
    s[i] = toupper(s[i]);
  }

  printf("Jälkeen muunnoksen: %s\n", s);

  return 0;
}
```

Tämä koodi ensin määrittää merkkijonon "s" arvon, ja käyttää sitten "for" silmukkaa ja "strlen" funktiota käydäkseen läpi jokaisen merkin merkkijonossa. Kutsumme sitten "toupper" funktiota muuttamaan jokaisen merkin suuraakkosiksi. Lopuksi tulostamme niin alkuperäisen kuin muunnetunkin merkkijonon konsoliin.

Ohjelman tuloste näyttää tältä:

```
Ennen muunnosta: Esimerkki teksti
Jälkeen muunnoksen: ESIMERKKI TEKSTI
```

## Syvempi sukellus

Vaikka <string.h> kirjastosta löytyvä "toupper" funktio on helppo ja tehokas tapa muuttaa merkkijonot suuraakkosiksi, on hyvä huomioida muutamia asioita. Ensinnäkin, tämä funktio ei tue muiden kielten kirjaimia, kuten skandinaavisia merkkejä. Tämä johtuu siitä, että C käsittelee vain ASCII merkkejä ja osa niistä on varattu aakkosille.

Toiseksi, silmukka, jota käytimme edellisessä koodiesimerkissä, käy läpi kaikki merkit jokaisen merkkijonon kohdalla, vaikka kaikkien merkkien ei tarvitse olla pieniä kirjaimia. Tämä voi olla hukkaa, jos merkkijonossa on paljon merkkejä. Tässä tapauksessa voimme käyttää "strlwr" funktiota, joka tekee kaikista merkeistä pieniä kirjaimia, ja sen jälkeen "strupr" funktiota, joka muuttaa kaikki merkit suuraakkosiksi.

## Katso myös

- [toupper funktio (C Reference)](https://www.cplusplus.com/reference/cctype/toupper/)
- [string.h kirjasto (The GNU C Library)](https://www.gnu.org/software/libc/manual/html_node/String-Library.html)
- [ASCII Taulukko (ASCII Codes)](https://www.ascii-code.com/)