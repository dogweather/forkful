---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Säännölliset lausekkeet (regex) auttavat tekstissä etsimisessä ja manipuloinnissa. Niitä käytetään, koska ne tekevät monimutkaisista kuvioista tunnistamisen ja korvaamisen nopeaksi ja helpoksi.

## How to:
```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int reti;
    char msgbuf[100];
    
    // Kompiloidaan säännöllinen lauseke
    reti = regcomp(&regex, "^a[[:alnum:]]", 0);
    if (reti) {
        fprintf(stderr, "Ei voitu kompiloida regexiä\n");
        return 1;
    }

    // Suoritetaan regex vertailu
    reti = regexec(&regex, "abc123", 0, NULL, 0);
    if (!reti) {
        puts("Sopii!");
    }
    else if (reti == REG_NOMATCH) {
        puts("Ei vastaa.");
    }
    else {
        regerror(reti, &regex, msgbuf, sizeof(msgbuf));
        fprintf(stderr, "Regex virhe vertailussa: %s\n", msgbuf);
        return 1;
    }

    // Vapautetaan varattu muisti regexille
    regfree(&regex);
    return 0;
}
```
Output:
```
Sopii!
```

## Deep Dive
Säännölliset lausekkeet juontavat juurensa teoreettiseen tietojenkäsittelytieteeseen 1950-luvulta. Vaihtoehtoja niille tarjoavat kuvioiden tunnistamiseen tarkoitetut kirjastot tai kielet, kuten SQL 'LIKE' tai XPath. C-kielessä POSIX regex -kirjasto tarjoaa työkalut säännöllisten lausekkeiden käsittelyyn.

## See Also
- POSIX regex manuaali: http://man7.org/linux/man-pages/man7/regex.7.html
- Tutorial muiden regex-funktioiden käyttöstä: https://www.regular-expressions.info/posix.html
- Säännöllisten lausekkeiden harjoittelutyökalu: https://regex101.com/
