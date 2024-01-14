---
title:    "C++: Päivämäärän haku"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Miksi 

Monissa ohjelmoinnin projekteissa on tarvetta käyttää nykyistä päivämäärää ja aikaa. Tämä voi olla hyödyllistä esimerkiksi laskenta- tai ajoitussovelluksissa. Tässä blogikirjoituksessa kerromme, miten saat nykyisen päivämäärän C++ -ohjelmassasi ja annamme muutamia esimerkkejä sen käytöstä.

# Miten

Käytännössä nykyisen päivämäärän saaminen C++:ssa on hyvin yksinkertaista. Sinun tulee vain sisällyttää <ctime> -kirjasto ohjelmaasi ja käyttää time_t ja tm -rakenteita. Tässä esimerkissä tulostamme nykyisen päivämäärän muodossa "DD/MM/YYYY":

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main()
{
    // Hae nykyinen aika
    time_t now = time(0);
    
    // Luo aikarakenteen ja sijoita nykyinen aika siihen
    tm *currentTime = localtime(&now);
    
    // Tulosta päivämäärä halutussa muodossa
    cout << "Tänään on " << currentTime->tm_mday << "/" << currentTime->tm_mon + 1 << "/" << currentTime->tm_year + 1900 << endl;
    
    return 0;
}
```

Tämän ohjelman tuloste olisi esimerkiksi:

```
Tänään on 13/10/2021
```

Voit myös käyttää muita muotoilumerkkejä muodostaaksesi päivämäärän haluamassasi muodossa. Esimerkiksi, jos haluat tulostaa kuukauden kirjaimellisena versiona, voit käyttää muotoilumerkkiä "%b" seuraavasti:

```C++
// Tulosta kuukausi kirjaimellisessa muodossa
cout << "Tänään on " << currentTime->tm_mday << " " << currentTime->tm_mon % 12 + 1 << " " << currentTime->tm_year + 1900 << endl;
```

Tämän ohjelman tuloste voisi olla esimerkiksi:

```
Tänään on 13 lokakuu 2021 
```

# Syvällisempi sukellus

Nykyisen päivämäärän saamiseen vaadittavat toimenpiteet vaihtelevat eri järjestelmien välillä, kuten Windowsilla, Linuxilla ja Macilla. Tämä johtuu siitä, että jokaisella järjestelmällä voi olla hieman erilainen ajanmittauslogiikka.

Päivämäärän ja ajan hallinta C++:ssa perustuu Unix-aikaleimaan, joka ilmaisee ajan kulun 1. tammikuuta 1970 kello 00:00:00 UTC:sta lähtien. Tämä aikaleima tallennetaan yleensä 32-bittiseen kokonaislukumuuttujaan, time_t:iin, ja sitä käytetään kaikissa päivämäärän ja ajan laskelmissa ja muunnoksissa. Tarkemmat tiedot ajanhallinnasta ja muunnoksista löytyvät <ctime> -kirjaston dokumentaatiosta.

# Katso myös

- C++ <ctime> kirjasto: https://www.cplusplus.com/reference/ctime/
- Unix-aikaleima: https://en.wikipedia.org/wiki/Unix_time