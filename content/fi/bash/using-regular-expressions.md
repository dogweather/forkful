---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
simple_title:         "Säännöllisten lausekkeiden käyttö"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regulääri-ilmaisut on hakujen ja tekstinkäsittelyn työkalu. Käyttäjät voivat nopeasti etsiä, korvata tai validoida tekstiä koodissa.

## How to:
```
# Etsi sanoja, jotka alkavat "ha" ja päättyvät "ku"
echo "haku helikopteri hattu hernekuja" | grep -o '\bha\w*ku\b'

# Tulostus:
haku
hernekuja

# Etsi kaikki riviä, joihin ei sisälly sana "koira"
echo -e "kissa\nkoira\nhamsteri" | grep -v "koira"

# Tulostus:
kissa
hamsteri
```

## Deep Dive
Regulääri-ilmaisut juontavat juurensa teoreettisesta automaattiteoriasta ja formaalikielistä. Vaihtoehtoina ovat mm. pelkkä tekstihaku tai parserit. Bash käyttää useimmiten POSIX-regulääri-ilmaisun syntaksia, mutta moderneissa työkaluissa kuten `grep` on myös laajennettu tuki (esim. PCRE).

## See Also
- GNU grep manuaali: [https://www.gnu.org/software/grep/manual/grep.html](https://www.gnu.org/software/grep/manual/grep.html)
- Regex101, testaa regulääri-ilmaisuja: [https://regex101.com](https://regex101.com)
- Regulääri-ilmaisujen pikakurssi: [https://regexone.com](https://regexone.com)
