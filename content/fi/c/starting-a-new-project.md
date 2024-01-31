---
title:                "Uuden projektin aloittaminen"
date:                  2024-01-20T18:03:10.723126-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uuden projektin aloittaminen"

category:             "C"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä ja Miksi?
Aloitamme uuden projektin koodaamiseen, koska haluamme luoda jotain uutta tai ratkaista ongelman. Se on kuin tyhjän kankaan maalaamista koodilla.

## How to: - Kuinka:
```C
#include <stdio.h>

int main() {
    printf("Hei Maailma!\n");
    return 0;
}
```
Kun ajat tämän, näet:
```
Hei Maailma!
```

## Deep Dive - Syväsukellus
Projektin aloittaminen C-kielellä on muuttunut vuosien varrella. "Hello World" on perinteinen ensimmäinen ohjelma - se on tapa tuntea itsensä tekijäksi. Vaihtoehtoiset aloitustavat riippuvat tavoitteista: web-kehitys, systeemi ohjelmointi, ja niin edelleen. Yksinkertaisuus, kuten yllä, opettaa ohjelman rakennetta; `#include` tuodaan kirjastoja, `main()` on aloituspiste ja `printf()` ilmaisee tulosteen. Huomaa, että nykyinen C-standardi (C17/ISO/IEC 9899:2018) suosii `int main(void)` allekirjoitusta.

## See Also - Katso Myös
- C Standard - ISO/IEC 9899: http://www.open-std.org/JTC1/sc22/wg14/www/docs/n2310.pdf
- C Tutorial - https://www.learn-c.org/
- Stack Overflow C questions - https://stackoverflow.com/questions/tagged/c
