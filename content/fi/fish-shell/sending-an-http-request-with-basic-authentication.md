---
title:                "Perusvarmennuksella lähetetään http-pyyntö"
html_title:           "Fish Shell: Perusvarmennuksella lähetetään http-pyyntö"
simple_title:         "Perusvarmennuksella lähetetään http-pyyntö"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Tervetuloa lukemaan tämän artikkelin, jossa kerron sinulle, miksi ja miten voit lähettää HTTP-pyynnön perustason todennuksella Fish Shellin avulla. Ei turhia selityksiä, vaan suoraan asiaan!

## Miksi

Voi olla monia syitä, miksi haluat lähettää HTTP-pyynnön perustason todennuksella Fish Shellin avulla. Ehkä haluat tarkistaa käyttäjätunnuksesi ja salasanasi tiettyyn palveluun tai ehkä haluat tehdä tietokantakyselyn, joka vaatii todennusta. Joka tapauksessa, tämä on hyödyllinen taito hallita, sillä lähes kaikki web-sovellukset käyttävät HTTP-pyyntöjä joten voit laajentaa osaamistasi monille eri alueille.

## Miten

Ensimmäinen askel on asentaa Fish Shellin HTTP-pyyntöjen hallintaan tarkoitettu lisäosa nimeltään [HTTPie](https://httpie.org/). Voit tehdä sen helposti käyttämällä `fisher`-pakettienhallintajärjestelmää. Jos et ole vielä asentanut `fisher`-ohjelmaa, voit tehdä sen komennolla:

```Fish Shell
curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
```

Seuraavaksi voit asentaa `httpie`-lisäosan komentamalla:

```Fish Shell
fisher install jethrokuan/httpie.fish
```

Nyt kun sinulla on `httpie`-lisäosa, voit käyttää sitä lähettämään HTTP-pyynnön perustason todennuksella. Alla on esimerkki, jossa lähetämme pyynnön Githubin API:lle käyttämällä käyttäjätunnustamme ja salasanaamme:

```Fish Shell
http -a username:password GET https://api.github.com/user
```

Ja tässä on oletettu vastaus:

```Fish Shell
HTTP/1.1 200 OK
Connection: close
Content-Type: application/json; charset=utf-8
...
{
  "login": "myusername",
  "id": 123456,
  ...
}
```

Voit myös käyttää `httpie`-lisäosan muita vaihtoehtoja, kuten `--form`-lippua jos haluat lähettää muotoiltuja tietoja tai `--json`-lippua jos haluat lähettää JSON-dataa. Voit lisätä niitä komentoon, esimerkiksi:

```Fish Shell
http -a username:password POST https://example.com/submit-form --form name=John --form age=25
```

## Deep Dive

HTTP-pyynnön lähettäminen perustason todennuksella Fish Shellillä voi olla hyödyllistä jos haluat tehdä automaattisia tehtäviä web-palveluissa, kuten päivittää sivun tai hakea tietokantatietoja. Voit myös käyttää muita lisäosia, kuten [jq](https://stedolan.github.io/jq/) tai [bass](https://github.com/edc/bass), helpottamaan tiedon käsittelyä vastauksista.

HTTPie-lisäosa tarjoaa myös muita toimintoja, kuten tiedostojen lähettämisen ja vastausten tallentamisen tiedostoihin. Voit lukea lisätietoja [HTTPie:n dokumentoinnista](https://httpie.org/doc).

## Katso myös

- [Opas Fish Shellin käyttöön aloittelijoille](https://medium.com/@Reiko82/fish-shell-opas-k%C3%A4ytt%C3%B6%C3%B6n-aloittelijoille-b7b7618b1001)
- [Fisherman-pakettienhallintajärjestelmä](https://github.com/fisherman/fisherman)
- [Fish Shellin