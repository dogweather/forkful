---
title:                "Elixir: Aloittaminen uudessa projektissa"
programming_language: "Elixir"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi aloittaa uusi projekti Elixir-ohjelmoinnissa?

Uuden ohjelmointiprojektin aloittaminen voi olla jännittävä ja haastava prosessi. Elixir-ohjelmointikielen avulla voit kuitenkin luoda tehokkaita ja skaalautuvia sovelluksia, jotka ovat helppoja ylläpitää ja laajentaa. Elixir tarjoaa myös monia hyödyllisiä ominaisuuksia, kuten vahvan ja dynaamisen tyyppijärjestelmän, virheettömän samanaikaisuuden ja helpon moduulirakenteen. Näiden etujen ansiosta Elixir on yksi suosituimmista ohjelmointikielistä uusien projektien aloittamiseen.

## Kuinka aloittaa uusi projekti Elixir-ohjelmoinnissa?

```Elixir
defmodule Hello do
  def say_hello(name) do
    "Hei #{name}!"
  end
end

IO.puts Hello.say_hello("Maailma") #=> "Hei Maailma!"
```

Aloita uusi projekti Elixirissä luomalla uusi moduuli ```defmodule``` -käskyllä. Sisälläsi voit määrittää erilaisia toimintoja, jotka voidaan kutsua toisiinsa. Esimerkissä näkemyksessämme luomme yksinkertaisen moduulin nimeltä ```Hello``` ja sen sisällä olevan toiminnon ```say_hello```, joka tulostaa tervehdyksen parametrina annetulle nimelle. Voit suorittaa tämän koodin komentoriviltä käyttämällä ```mix run -e "Hello.say_hello('Maailma')"```.

## Syvällisempi sukellus uuden projektin aloittamiseen Elixir-ohjelmoinnissa

Uuden projektin aloittaminen Elixirissä vaatii useita eri vaiheita, mutta onneksi Elixir-yhteisö on luonut monia resursseja ja työkaluja helpottamaan tätä prosessia. Voit esimerkiksi käyttää Mix-työkalua luomaan uuden Elixir-projektin käyttämällä komentoa ```mix new <projektin_nimi>```. Tämä luo valmiin projektin rakenteen ja tarvittavat tiedostot, joten voit keskittyä koodaamiseen.

Toinen hyödyllinen työkalu uuden projektin aloittamiseen on Elixirin sisäänrakennettu dokumentaatio. Dokumentaatio sisältää yksityiskohtaisia ohjeita eri toimintojen ja kirjastojen käytöstä sekä esimerkkejä koodin käytöstä. Voit käyttää dokumentaatiota referenssinä löytääksesi sopivia kirjastoja tai ymmärtääksesi paremmin Elixirin syntaksia ja käytäntöjä.

## Katso myös

- [Elixirin virallinen dokumentaatio](https://hexdocs.pm/elixir/)
- [Elixir-kurssit ja opetusohjelmat](https://elixirschool.com/)
- [Elixir-yhteisöfoorumi](https://elixirforum.com/)