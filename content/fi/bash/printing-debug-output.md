---
title:                "Virheenjäljitystulosteiden tulostaminen"
date:                  2024-01-20T17:52:10.142316-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"

category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? | Mikä & Miksi?
Debug-tulosteiden tulostaminen on viestien näyttämistä konsolissa, joka auttaa koodin toiminnan ymmärtämisessä. Ohjelmoijat tekevät sitä, koska se on nopea tapa löytää ja ratkaista ongelmat koodissa.

## How to: | Kuinka:
```Bash
# Yksinkertainen debug-viesti
echo "Debug: muuttujan arvo on $muuttuja"

# Ehtolauseissa käytettävä debug
if [ "$muuttuja" -eq 10 ]; then
    echo "Debug: muuttuja on tasan 10"
fi

# Silmukoiden debuggaus
for i in {1..5}; do
    echo "Debug: i:n arvo on nyt $i"
done
```
Esimerkkituloste:
```
Debug: muuttujan arvo on 5
Debug: muuttuja on tasan 10
Debug: i:n arvo on nyt 1
Debug: i:n arvo on nyt 2
Debug: i:n arvo on nyt 3
Debug: i:n arvo on nyt 4
Debug: i:n arvo on nyt 5
```

## Deep Dive | Syväsukellus:
Varhaiset ohjelmoijat alkoivat tulostaa viestejä koodistaan seuratakseen sen käyttäytymistä ennen virheenjäljitystyökalujen yleistymistä. Vaihtoehtoja on monia: lokeihin kirjaaminen, järjestelmävalvojan työkalut tai kehittyneemmät debuggaustyökalut. Bash-komentorivillä `echo` on yksinkertainen mutta tehokas tapa välittää debug-tietoja. Sen lisäksi käytössä voi olla `-x` vaihtoehto, joka tulostaa jokaisen komennon ja sen argumentit ennen suoritusta.

## See Also | Katso Myös:
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Bash manuaali: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html
- Debugging Bash scripts: https://www.shellscript.sh/debugging.html
