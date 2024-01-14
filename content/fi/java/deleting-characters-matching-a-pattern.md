---
title:                "Java: Mallia vastaavien merkkien poistaminen"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi?

Joillakin ohjelmankehittäjillä saatetaan olla tilanteita, joissa on tarpeen poistaa merkkejä tietyllä kaavalla. Tämä voi olla esimerkiksi siinä tapauksessa, että halutaan muokata tai analysoida tekstidataa, jossa on ylimääräisiä tai virheellisiä merkkejä.

## Miten tehdä?

Tässä ohjelmointiblogissa esitellään erilaisia tapoja poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Ensimmäinen vaihtoehto on käyttää String-luokan replaceAll-metodia, joka hyödyntää säännöllisiä lausekkeita. Toinen vaihtoehto on käyttää StringBuilder-luokan deleteCharAt-metodia, joka poistaa merkin annetusta indeksistä. Näiden lisäksi on olemassa myös muita tapoja, mutta ne eivät välttämättä ole yhtä tehokkaita tai helppokäyttöisiä.

```Java 
// Esimerkki replaceAll-metodista 
String teksti = "Tämä on esimerkkiteksti, josta halutaan poistaa kaikki numerot 12345";
String uusiTeksti = teksti.replaceAll("\\d", ""); // poistaa kaikki numerot

System.out.println(uusiTeksti); // tulostaa "Tämä on esimerkkiteksti, josta halutaan poistaa kaikki numerot"

// Esimerkki deleteCharAt-metodista
StringBuilder teksti = new StringBuilder("Tämä on esimerkkiteksti, josta halutaan poistaa ensimmäinen merkki");
teksti.deleteCharAt(0); // poistaa ensimmäisen merkin

System.out.println(teksti.toString()); // tulostaa "ämä on esimerkkiteksti, josta halutaan poistaa ensimmäinen merkki"
```

## Syvemmälle pinnan alle

Säännölliset lausekkeet ovat tehokas työkalu kaavojen määrittämiseen ja merkkien poistamiseen. Niiden käyttö voi vaatia hieman harjoittelua, mutta ne tarjoavat paljon enemmän mahdollisuuksia verrattuna yksittäisten merkkien poistoon. On myös huomioitava, että säännölliset lausekkeet voivat olla hidas suurilla tekstimäärillä, joten tarvittaessa kannattaa harkita muita vaihtoehtoja.

## Katso myös

- Java String-luokan API: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
- Java StringBuilder-luokan API: https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html
- Säännölliset lausekkeet Java:ssa: https://docs.oracle.com/javase/tutorial/essential/regex/