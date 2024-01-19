---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Käytämme debug-tulostusta ohjelmakoodin suoritusvirheiden löytämiseksi. Tämä auttaa meitä ymmärtämään, mitä koodissamme tapahtuu suorituksen aikana, ja tunnistamaan mahdolliset ongelmat nopeammin.

## Kuinka näin:
Alla on esimerkkejä, kuinka luodaan debug-tulostusta Bash-skriptillä:

```Bash
#!/bin/bash

for i in {1..5}; do
  echo "Debug: Iteration number $i"
  # Muu koodi tähän
done
```

Esimerkistä tulostuu seuraavaa:
```Bash
Debug: Iteration number 1
Debug: Iteration number 2
Debug: Iteration number 3
Debug: Iteration number 4
Debug: Iteration number 5
```
## Syvempi sukellus:
Debug-tulostus on ollut käytössä jo ennen kuin UNICS-järjestelmä, Bashin esi-isi, kehitettiin. Alternatiiveista yleisimpiä ovat muun muassa logitiedostot ja erityiset debuggaustyökalut. Bashissa debug-tulostus voidaan ohjata `set -x` ja `set +x` komennoksilla, jolloin kaikki komennot tulostetaan ennen suoritusta.

## Katso myös:
1. Bash-skriptauksen perusteet: https://linuxhandbook.com/bash-scripting/
2. Bash-debugging: https://www.gnu.org/software/bash/manual/html_node/Debugging.html
3. Bashin virhetarkistus ja debuggaus: https://ryanstutorials.net/bash-scripting-tutorial/bash-debugging.php

Muista, että debug-tulostuksen tarkoitus on auttaa sinua koodin kirjoitusprosessissa. Se on yksi monista työkaluista, ja sen tarpeellisuus riippuu tilanteestasi. Hyvää koodausta!