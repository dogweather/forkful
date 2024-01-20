---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "C: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Tarkistetaanko hakemisto olemassa?

## Mikä & Miksi?

Hakemiston olemassaolon tarkistaminen tarkoittaa sitä, että ohjelmoija varmistaa, onko tietty polku järjestelmässä olemassa vai ei. Tämä on tarpeen, jotta voidaan välttää virheitä, jotka voisivat tapahtua, jos yritämme käyttää olematonta hakemistoa.

## Miten:

```C
#include <unistd.h>

int main()
{
    if( access( "/jotakin/hakemistoa", F_OK ) != -1 ) {
        // hakemisto on olemassa
    } else {
        // hakemisto ei ole olemassa
    }
    return 0;
}
```

Näyte tuloste:

```C
// Jos hakemisto on olemassa:

// Jos hakemisto ei ole olemassa:
```

## Syvempi sukellus

Tarkistaakseen, onko hakemisto olemassa, ohjelmoijat ovat käyttäneet erilaisia metodeja koko C-kielen historian ajan. Access()-funktio on yksi yleisimmistä menetelmistä, mutta sen sijaan jotkut ohjelmoijat saattavat suosia stat()-funktiota tai opendir()-funktiota. Vaikka nämä vaihtoehdot voivat vaikuttaa samanlaisilta, niillä on kuitenkin olennaisia eroja esimerkiksi toiminnan, tehokkuuden ja käytön suhteen.

## Katso myös

- *[C Tutorial: File I/O](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)*: Laajempi opas C:n tiedosto- ja hakemisto-operaatioihin.