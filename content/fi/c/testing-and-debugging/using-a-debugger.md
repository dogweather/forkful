---
title:                "Debuggerin käyttö"
aliases:
- /fi/c/using-a-debugger/
date:                  2024-02-03T18:10:22.395197-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debuggerin käyttö"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/using-a-debugger.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mitä & Miksi?

C-kielen debuggerit ovat erikoistyökaluja, jotka mahdollistavat ohjelmoijille koodin vaiheittaisen suorituksen seurannan, muuttujien tarkastelun ja suoritusvirran monitoroinnin. Tämä prosessi on olennainen osa virheiden tunnistamisessa ja korjaamisessa, varmistamassa, että koodi toimii odotetulla tavalla.

## Kuinka:

GDB (GNU Debugger) on yleisimmin käytetty debuggeri C-ohjelmoinnissa. Tässä on lyhyt opas GDB:n käyttöön yksinkertaisen C-ohjelman debuggaamiseen.

Ensin, käännä C-ohjelmasi `-g`-lipulla sisällyttääksesi debuggaustiedot:

```c
gcc -g program.c -o program
```

Seuraavaksi, käynnistä GDB kompiloidun ohjelmasi kanssa:

```bash
gdb ./program
```

Nyt voit käyttää erilaisia komentoja GDB:n sisällä sen toiminnan hallitsemiseksi. Tässä muutamia peruskomentoja:

- `break`: Aseta keskeytyskohta tietylle riville tai funktioon keskeyttääksesi suorituksen.
  - Esimerkki: `break 10` tai `break main`
- `run`: Aloita ohjelmasi suoritus GDB:n sisällä.
- `next`: Suorita seuraava koodirivi astumatta funktioihin.
- `step`: Suorita seuraava koodirivi astuen funktioihin.
- `print`: Näytä muuttujan arvo.
- `continue`: Jatka suoritusta seuraavaan keskeytyskohtaan asti.
- `quit`: Poistu GDB:stä.

Tässä esimerkkisessio yksinkertaisen ohjelman debuggaamisesta:

```c
#include <stdio.h>

int main() {
    int i;
    for (i = 0; i < 5; i++) {
        printf("%d\n", i);
    }
    return 0;
}
```

Käännä ja käynnistä GDB kuten kuvattu. Aseta keskeytyskohta `printf`-riville komennolla `break 5` ja sitten `run`. Käytä `next`-komentoa edetäksesi silmukan läpi ja `print i` tutkiaksesi silmukkamuuttujaa.

Esimerkkituloste keskeytyskohdan asettamisen ja ensimmäisen iteraation välillä:

```
Breakpoint 1, main () at program.c:5
5         printf("%d\n", i);
```

Käyttämällä `print i` muutaman iteraation jälkeen:

```
$3 = 2
```

Tämä osoittaa, miten tutkia yksinkertaisen ohjelman tilaa ja suoritusvirtaa.

## Syväsukellus

Debuggauksen käsite on kehittynyt merkittävästi ohjelmoinnin alkuaikojen jälkeen, jolloin fyysiset hyönteiset (kirjaimelliset bugit) saattoivat aiheuttaa ongelmia mekaanisissa tietokoneissa. Nykyään debuggerit, kuten GDB, tarjoavat edistyneitä ominaisuuksia perus askeltamisen ja muuttujien tarkastelun lisäksi, kuten käänteisdebuggaus (ohjelman suorittaminen taaksepäin), ehdolliset keskeytyskohdat ja skriptaus automatisoituja debuggaustehtäviä varten.

Vaikka GDB on tehokas ja laajalti käytetty, se voi olla tiheä ja haastava aloittelijoille. Vaihtoehtoiset debuggaustyökalut ja IDEt (Integroidut Kehitysympäristöt), kuten Visual Studio Code, CLion tai Eclipse, tarjoavat käyttäjäystävällisempiä käyttöliittymiä C-koodin debuggaamiseen, usein integroiden visuaalisia apuvälineitä ja intuitiivisempia hallintamekanismeja. Nämä vaihtoehdot eivät ehkä tarjoa GDB:n täyttä toiminnallisuuden syvyyttä, mutta voivat olla saavutettavampia C-ohjelmoinnin uusille tulokkaille.

Lisäksi kielipalvelinprotokollien ja debuggausstandardien esiinmarssi on helpottanut alustojen välisiä debuggausratkaisuja, tehden debuggauskokemuksesta johdonmukaisemman eri työkalujen ja ympäristöjen välillä. Huolimatta näistä edistysaskelista, perinteisen debuggerin, kuten GDB:n, käytön oppiminen tarjoaa korvaamatonta näkemystä C-ohjelmien suorituksesta ja pysyy olennaisena taitona kehittäjän työkalupakissa.
