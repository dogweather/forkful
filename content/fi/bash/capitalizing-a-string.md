---
title:                "Merkkijonon muuttaminen isoin kirjaimin"
html_title:           "Bash: Merkkijonon muuttaminen isoin kirjaimin"
simple_title:         "Merkkijonon muuttaminen isoin kirjaimin"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Mitä ja miksi?
Kapitalisaatio tarkoittaa merkkijonon ensimmäisen kirjaimen muuttamista isommaksi ja muun tekstin säilyttämistä ennallaan. Ohjelmoijat tekevät tämän helpottaakseen lukemista ja tehdäkseen koodista paremmin luettavaa. 

Kuinka:
```Bash
# Esimerkkejä merkkijonon kapitalisaatiosta

# Yksinkertainen esimerkki

string="tämä on esimerkki"
echo "Ennen kapitalisaatiota: $string"
string="${string^}"
echo "Jälkeen kapitalisaatiota: $string"

# Toisessa esimerkissä käytetään pipea

echo "exo kapitalisaatiota" | awk '{print toupper($1)}'

# Output: EXO KAPITALISAATIOTA
```

Deep Dive:
Kapitalisaatio ei ole vain Bash-ominaisuus, vaan se on peräisin Unix-käyttöjärjestelmästä, joka Bash perustuu. Tähän kuuluu myös munanvälityskomento, joka tekee saman isolla merkillä ensimmäisen kirjaimen ja muun tekstin kanssa. Vaihtoehto kapitalisaatiolle on käyttää sed-komentoa tai käydä läpi merkkijono loopin kautta.

## Katso myös:
- [Bash-opas](https://www.gnu.org/software/bash/manual/bash.html)
- [Komennot ja muuttujat - Bash-opas](https://www.tutorialspoint.com/unix/unix-shell-variables.htm)