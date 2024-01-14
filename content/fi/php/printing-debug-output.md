---
title:    "PHP: Ladonta virheenkorjaustulostetta"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi

Jos olet PHP-ohjelmoija, saatat törmätä tilanteisiin, joissa sinun täytyy tarkastella koodiasi lähemmin virheiden selvittämiseksi. Tässä tilanteessa debug-tulosteen tulostaminen voi olla erittäin hyödyllistä.

## Miten tehdä

Debug-tulosteen tulostaminen PHP:ssä on helppoa. Voit käyttää sisäänrakennettua "echo" -toimintoa, joka tulostaa haluamasi tiedon web-sivulle. Esimerkiksi:

```PHP
echo "Hello world!";
```

tulostaa tekstin "Hello world!" näytölle.

Voit myös tulostaa muuttujien arvoja nähdäksesi, miten ne muuttuvat koodin suorituksen aikana. Esimerkiksi:

```PHP
$teksti = "Tämä on tekstiä";
echo $teksti;
```

tulostaa tekstin "Tämä on tekstiä" näytölle.

Debug-tulosteen avulla voit myös tarkistaa ehtolauseiden ja silmukoiden suorituksen. Esimerkiksi:

```PHP
$i = 1;
while ($i < 10) {
    echo $i;
    $i++;
}
```

tulostaa luvut 1-9 näytölle.

## Syvempi sukellus

Debug-tulosteen tulostaminen ei ole vain hyödyllistä virheiden selvittämiseksi, vaan se voi myös auttaa sinua ymmärtämään koodisi suorituksen kulkua. Voit käyttää sitä apuna löytääksesi tehokkaampia ratkaisuja ja välttääksesi turhia virheitä tulevaisuudessa.

On myös hyvä muistaa, että debug-tulosteet ovat vain väliaikaisia. Ne eivät kuulu lopulliseen koodiin, joten muista poistaa ne ennen julkaisua.

## Katso myös

- [PHP:n viralliset dokumentaatiot debug-tulosteista](https://www.php.net/manual/en/function.echo.php)
- [Debug-tulosteen käyttö blogikirjoituksen kuvina](https://www.sean.co.uk/a/webdesign/debugging-output.shtml)