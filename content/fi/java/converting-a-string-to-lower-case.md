---
title:                "Merkkijonon muuntaminen pieneksi kirjoituksesi"
html_title:           "Java: Merkkijonon muuntaminen pieneksi kirjoituksesi"
simple_title:         "Merkkijonon muuntaminen pieneksi kirjoituksesi"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Mikä & Miksi?

Miksi muuttaa merkkijono pienaakkosiksi? Merkkijonon muuttaminen pienaakkosiksi on yleinen tehtävä ohjelmoinnissa, jossa merkkijonoja käsitellään. Pienaakkosiksi muuttamalla merkkijonoja voidaan helpommin vertailla ja käsitellä, sillä pienet ja isot kirjaimet eivät enää häiritse vertailua.

Miten tehdään:

```Java
String s = "TÄMÄ ON MERKKIJONO"; // Luodaan merkkijono s

System.out.println(s.toLowerCase()); // Tulostetaan merkkijono pienaakkosina
```

Tuloste:

```Java
tämä on merkkijono
```

Oletetaan, että merkkijono sisältää jo alunperinkin pieniä kirjaimia, eikä niiden muuttuminen pienaakkosiksi ole tärkeää. Silloin voidaan käyttää metodia **toLowerCase()** varmuuden vuoksi ja varmistaa, että kaikki kirjaimet ovat pieniä.

```Java
String s = "tämä on pieni merkkijono";

System.out.println(s.toLowerCase()); // Tulostetaan merkkijono pienaakkosina
```

Tuloste:

```Java
tämä on pieni merkkijono
```

Deep Dive:

Historiallisessa kontekstissa merkkijonon muuttaminen pienaakkosiksi ei ole ollut aina niin yksinkertaista. Esimerkiksi ASCII-koodissa ei ole ollut erillisiä koodiarvoja pienille ja isoille kirjaimille, jolloin muuttaminen on vaatinut erillisiä toimenpiteitä. Nykyään Java-kielessä tämä prosessi on paljon helpompaa, sillä kielessä on valmiina metodi **toLowerCase()**.

On myös olemassa muita tapoja muuttaa merkkijono pienaakkosiksi, kuten käyttämällä **CharSequence**-rajapintaa tai käyttämällä esimerkiksi **for**-silmukkaa. Kuitenkin metodi **toLowerCase()** on yksi helpoimmista ja tehokkaimmista tavoista tehdä tämä toimenpide.

Tarkemmat tiedot muuttamisprosessista löytyvät Java-dokumentaatiosta.

Katso myös:

[Java - String toLowerCase() Method](https://www.tutorialspoint.com/java/java_string_tolowercase.htm)