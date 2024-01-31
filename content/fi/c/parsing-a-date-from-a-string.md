---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:35:03.446925-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Stringistä päivämäärän parsiminen tarkoittaa tekstissä esitetyn päivämäärän muuttamista ohjelman käsiteltävään muotoon. Sitä tarvitaan, jotta voidaan käsitellä ja verrata päivämääriä, tehdä muunnoksia tai tallentaa tietokantoihin järkevästi.

## How to (Kuinka tehdä):
```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    const char *date_str = "2023-04-07";
    struct tm tm;
    if (strptime(date_str, "%Y-%m-%d", &tm)) {
        printf("Parsed date: %04d-%02d-%02d\n", tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);
    } else {
        printf("Failed to parse date.\n");
    }
    return 0;
}
```
Output:
```
Parsed date: 2023-04-07
```

## Deep Dive (Syväsukellus):
Historiallisesti päivämäärän parsiminen on ollut ohjelmistokehittäjien yleinen päänsärky, koska päivämäärämuotoja on monia ja ne vaihtelevat kulttuureittain. C-kielen standardikirjasto tarjoaa `strptime`-funktion päivämäärien käsittelyyn, mutta se ei ole aina ollut osa standardia ja sen toteutus voi vaihdella.

Vaihtoehtoisesti päivämäärän voi parsia itse määrittelemällässä omat säännöt ja käyttämällä stringin käsittelyyn tarkoitettuja funktioita kuten `sscanf` tai manuaalista käsittelyä. Tämä voi olla tarpeen, jos työskennellään alustoilla, joilla `strptime` ei ole saatavilla.

Toteutuksessa täytyy ottaa huomioon erilaiset päivämääräformaattien noudattamat standardit, kuten ISO 8601 (`YYYY-MM-DD`), joka on käytössä esimerkikissämme. On myös tärkeää varautua virheen käsittelyyn ja varmistaa, ettei virheellinen data aiheuta ongelmia ohjelman toiminnassa.

## See Also (Katso myös):
- C11 standardin `strptime`-funktion määrittely: https://en.cppreference.com/w/c/chrono/strptime
- ISO 8601 päivämäärä- ja aikaformaatin standardi: https://www.iso.org/iso-8601-date-and-time-format.html
- `strftime`-funktio päivämäärien formatointiin: https://en.cppreference.com/w/c/chrono/strftime
