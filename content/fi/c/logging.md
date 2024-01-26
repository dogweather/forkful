---
title:                "Lokitus"
date:                  2024-01-26T01:00:33.411526-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lokitus"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/logging.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lokitus on käytännössä ohjelman toiminnan kirjaamista ylös, tyypillisesti viestien kirjoittamista tiedostoon tai terminaaliin. Ohjelmoijat tekevät sen tapahtumien seurannan, ongelmien diagnosoinnin ja auditointipolkujen, jotka kertovat sovelluksen toiminnasta ajan yli, ylläpitämiseksi.

## Miten toimitaan:
Aloitetaan joistakin perusasioista. C:ssä ei ole sisäänrakennettua lokituskehystä, mutta voit tehdä yksinkertaisen ratkaisun käyttämällä `stdio.h`-kirjastoa. Tässä miten:

```c
#include <stdio.h>
#include <time.h>

void logMessage(const char* message) {
    time_t now;
    time(&now);
    char *date = ctime(&now);
    date[strlen(date) - 1] = '\0'; // Poista rivinvaihto ctime()-funktion tuloksen lopusta
    printf("[%s] %s\n", date, message);
}

int main() {
    logMessage("Sovellus on käynnistynyt.");
    // ... koodisi tulee tähän ...
    logMessage("Sovellus tekee jotain tärkeää.");
    // ... koodisi jatkuu ...
    logMessage("Sovellus on päättynyt.");
    return 0;
}
```

Esimerkkituloste voisi näyttää tältä:

```
[Ti Mar 9 12:00:01 2023] Sovellus on käynnistynyt.
[Ti Mar 9 12:00:02 2023] Sovellus tekee jotain tärkeää.
[Ti Mar 9 12:00:03 2023] Sovellus on päättynyt.
```

Tietenkin todellisessa maailmassa haluaisit luultavasti kirjoittaa tiedostoon terminaalin sijaan, käsitellä erilaisia lokitasoja ja ehkä käyttää valmiiksi määriteltyä kirjastoa.

## Syväsukellus
Lokitus C:ssä on viehättävää vanhanaikaisuutta—se on yhtä matalan tason kuin suurin osa kielestä muutenkin. Historiallisesti lokitusta tehtiin käyttäen `fprintf` toimintoa `stderr` kanssa tai tiedostoon kirjoittamiseksi. Ohjelmien monimutkaistuessa myös lokitustarpeet kasvoivat, mikä johti `syslog`-kaltaisten kirjastojen kehittämiseen Unix-järjestelmissä, jotka pystyivät käsittelemään lokitusta monista lähteistä erilaisten tärkeysasteiden kanssa.

Nykyisessä maisemassa on runsaasti C-lokituskirjastoja, kuten `zlog`, `log4c` ja `glog`, jotka tarjoavat rikkaan toiminnallisuuden joukon, kuten lokirotatiot, rakenteellisen lokituksen ja monisäikeisen lokituksen. Nämä ratkaisut mahdollistavat yksityiskohtaisen hallinnan lokien verbositeetin, määränpäiden ja muotojen suhteen.

Lokitusjärjestelmän toteuttamisessa yksityiskohtia, kuten aikaleiman muotoilu, lokitiedostojen hallinta ja suorituskyky, on harkittava. Lokien aikaleimaaminen on kriittistä tapahtumien yhdistämiseksi, kun taas lokirotaatio varmistaa, etteivät lokitiedostot kuluta liikaa levytilaa. Lokien kirjoittamisen tulisi myös olla nopeaa ja mainflowta estämätöntä, jotta lokitus ei muodostu pullonkaulaksi.

## Katso Lisäksi
Sukeltaaksesi syvemmälle C:n lokituskirjastoihin ja -käytäntöihin, tarkista nämä resurssit:

- GNU `syslog` manuaali: https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- `zlog`: Erittäin mukautettava lokituskirjasto C:lle - https://github.com/HardySimpson/zlog
- `log4c`: Lokitusaarkehikko C:lle, mallinnettuna Log4j:n jälkeen - http://log4c.sourceforge.net/
- `glog`: Googlen sovellustason lokituskirjasto - https://github.com/google/glog