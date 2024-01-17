---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Javascript: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Mikä & Miksi?

Tarkistaa, onko kansio olemassa, on tärkeä osa ohjelmointia. Tämä tarkistus auttaa varmistamaan, että ohjelma ei kohtaa odottamatonta virhettä yrittäessään käyttää kansiota, joka ei ole olemassa tai johon käyttöoikeudet eivät riitä.

Miten:

```Javascript
if (fs.existsSync(path)) {
  console.log("Hakemisto löytyy!");
} else {
  console.log("Hakemistoa ei löytynyt :(");
}
```

Tämä koodiesimerkki käyttää Node.js:n sisäänrakennettua fs-moduulia tarkistaakseen, onko annetussa polussa oleva hakemisto olemassa. Jos hakemisto löytyy, tulostetaan "Hakemisto löytyy!" ja muussa tapauksessa "Hakemistoa ei löytynyt :(". 

Syvemmälle:
Tarkistus, onko kansio olemassa, on tärkeä osa tiedostojärjestelmän hallintaa. Se auttaa varmistamaan, että ohjelma ei yritä käyttää kansiota, joka ei ole käytettävissä, mikä voi johtaa odottamattomiin virheisiin. Aiemmin, ennen Node.js:ää, sama toiminnallisuus saavutettiin esimerkiksi Perl-ohjelmointikielellä käyttäen stat-funktiota tiedoston olemassaolon tarkistamiseen.

On myös muita tapoja tarkistaa, onko kansio olemassa. Yksi vaihtoehto on käyttää try-catch-rakennetta, jossa yritetään käyttää kansiota ja käsitellään virhe, jos kansiota ei löydy. Toinen vaihtoehto on käyttää npm-pakettia kuten "fs-extra", joka tarjoaa yksinkertaisemman tavan tarkistaa tiedoston tai kansion tyyppi.

Katso myös: 
- https://nodejs.org/api/fs.html#fs_fs_existssync_path
- https://www.npmjs.com/package/fs-extra
- https://perldoc.perl.org/functions/stat.html