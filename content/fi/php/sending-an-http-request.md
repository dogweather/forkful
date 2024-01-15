---
title:                "Lähettämällä http-pyyntö"
html_title:           "PHP: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi
Kun rakennat verkkosivustoja tai sovelluksia, sinun on usein tarpeen tehdä tietopyyntöjä toisille verkkosivuille tai palvelimille. Näiden tietopyyntöjen avulla voit hakea tai lähettää tietoja ja samalla hyödyntää monia erilaisia palveluita ja rajapintoja. PHP:n avulla voit helposti lähettää HTTP-pyyntöjä ja käsitellä vastaanotettua dataa.

## Kuinka
HTTP-pyyntöjen lähettäminen PHP:ssa on yksinkertaista. Voit käyttää siihen built-in funktiota `file_get_contents()` tarvittavan URL:n kanssa. Tämä funktio hakee kaiken vastaanotetun datan ja palauttaa sen merkkijonona. Esimerkiksi seuraava koodi hakee GitHubin rajapinnasta listan kaikista käyttäjän repositorioista ja tulostaa sen näytölle:

```PHP
$url = "https://api.github.com/users/käyttäjänimi/repos";
$data = file_get_contents($url);
echo $data;
```

Tuloksena saatava data on JSON-muodossa, joten voit käyttää `json_decode()`-funktiota muuttamaan sen PHP:n taulukoksi ja käsittelemään sitä helposti. Esimerkiksi voit tulostaa kaikkien repositorioiden nimet seuraavalla tavalla:

```PHP
$url = "https://api.github.com/users/käyttäjänimi/repos";
$data = file_get_contents($url);
$repos = json_decode($data, true); //luodaan PHP-taulukko
foreach ($repos as $repo) {
    echo $repo['name'] . "\n";
}
```

## Syväsukellus
Voit lähettää HTTP-pyyntöjä myös muiden HTTP-metodien avulla, kuten POST ja PUT. Tämä tapahtuu samaan tapaan kuin GET metodi, mutta nyt voit määrittää myös pyynnön kohdeosoitteen, headerit ja pyyntöparametrit.

```PHP
$url = "https://api.example.com/post_endpoint";
$data = ['name' => 'John', 'age' => 30, 'city' => 'Helsinki'];
$options = [
    'http' => [
        'method' => 'POST',
        'header' => "Content-type: application/json\r\n",
        'content' => json_encode($data)
    ]
];
$context = stream_context_create($options);
$result = file_get_contents($url, false, $context);
var_dump($result);
```

Tässä tapauksessa olemme lähettäneet POST-pyynnön ja määrittäneet headerin ja pyyntöparametrit. Vastaavasti voit käyttää `stream_context_create()`-funktiota lähettämään muita HTTP-metodeja ja lisäämään tarvittavat parametrit pyyntöön.

## Katso myös
- [PHP:n virallinen dokumentaatio HTTP-funktioista](https://www.php.net/manual/en/book.http.php)
- [HTTP-pyynnön lähetys PHP:ssa - tutoriaali](https://www.w3schools.com/php/php_http.asp)
- [HTTP-pyynnön lähettäminen käyttämällä CURL kirjastoa](https://www.php.net/manual/en/book.curl.php)