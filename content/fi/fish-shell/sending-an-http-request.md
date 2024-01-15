---
title:                "Lähettämällä http-pyyntö"
html_title:           "Fish Shell: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit lähettää HTTP-pyynnön Fish Shellillä? Yksinkertaistaen, se antaa sinulle pääsyn verkkoon ja sieltä saatavaan tietoon suoraan komentokehotteesta. Se on nopea ja tehokas tapa kommunikoida internetin kanssa ja voi olla hyödyllinen monissa eri skenaarioissa, kuten hakkeroinnissa tai tiedonkeruussa.

## Kuinka tehdä

Ensimmäiseksi tarvitset Fish Shellin ja sen uusimman version. Voit ladata sen omalle tietokoneellesi osoitteesta www.fishshell.com. Kun olet asentanut sen, voit aloittaa lähettämään HTTP-pyynnöksiä.

```Fish Shell
$url = "https://www.example.com"
$response = curl -X GET $url
echo $response
```

Tässä esimerkissä luomme muuttujan nimeltä `$url`, joka sisältää verkkosivuston osoitteen. Sitten käytämme `curl`-komentoa lähettämään GET-pyynnön tälle sivustolle ja tallennamme vastauksen muuttujaan `$response`. Lopuksi käytämme `echo`-komentoa tulostamaan vastauksen terminaalissa.

Voit käyttää myös muita HTTP-metodeja, kuten POST, PUT tai DELETE, muuttamalla `curl`-komennon parametreja. Voit myös lisätä muita parametreja, kuten otsikoita tai tietoja pyynnön mukana. Varoituksena, että Fish Shellin `curl`-komennossa ei ole kaikkia samoja ominaisuuksia kuin perinteisessä curl-ohjelmassa.

## Syvemmälle sukeltaminen

Fish Shellin `curl`-komennossa käytetään taustalla `libcurl`-kirjastoa, joka on suunniteltu toimimaan monilla eri protokollilla, kuten HTTP ja HTTPS. Voit lukea lisää tästä kirjastosta osoitteessa https://curl.haxx.se/docs/manpage.html ja löytää kaikki mahdolliset parametrit ja vaihtoehdot.

Voit myös käyttää muita Fish Shellin sisäänrakennettuja käskyjä lähettämään HTTP-pyynnön, kuten `wget` ja `fetch`. Ne ovat hyviä vaihtoehtoja, jos et tarvitse kaikkia curlin ominaisuuksia.

## Katso myös

- Fish Shellin virallinen verkkosivusto: www.fishshell.com
- `libcurl`-kirjaston dokumentaatio: https://curl.haxx.se/docs/manpage.html
- Lisää käskyjä Fish Shellissä: https://fishshell.com/docs/current/commands.html