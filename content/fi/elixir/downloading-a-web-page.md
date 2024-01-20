---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Elixirin käyttö verkkosivun lataamiseen

## Mikä & Miksi?

Verkkosivun lataaminen on prosessi, jossa tietokone hakee ja tallentaa web-sivun tiedot. Ohjelmoijat tekevät tämän, koska sen avulla he voivat käsitellä ja analysoida verkkosisältöä automaattisesti.

## Miten:

Esimerkkilähdekoodimme käyttää `HTTPoison`-kirjastoa verkkosivun lataamiseen.

1. Asentaa lähdepaketti:

    ```elixir
    mix deps.get httpoison
    ```

2. Lataa verkkosivua:

    ```elixir
    def download_page(url) do
      case HTTPoison.get(url) do
        {:ok, response} ->
          {:ok, response.body}
        {:error, reason} ->
          {:error, reason}
      end
    end
    ```

Kokeillaan seuraavaa URL-osoitetta.

    ```elixir
    download_page("https://www.example.com")
    ```

Tulostuu sivun HTML-koodi tai virheilmoitus.

## Syvä sukellus:

Verkkosivun lataaminen on ollut osa verkosto-ohjelman ominaisuuksia alusta lähtien. Tämä on sen vuoksi, että verkkosivu tarjoaa valmiiksi muotoiltua tietoa, jota ohjelmat voivat käyttää.

Vaihtoehtoisia tapoja verkkosivujen lataamiseen Elixirissä ovat `HTTPotion` ja `Tesla`, jotka tarjoavat erilaisia ominaisuuksia ja äänensävyjä.

`HTTPoison` käyttää `hackney`-kirjastoa HTTP-pyyntöjen tekemiseen. Se tarjoaa suoraviivaisen tavan käsitellä HTTP-pyyntöjä ja vastauksia, ja sitä on helppo käyttää yhdessä Elixirin muiden ominaisuuksien kanssa.

## Lisätietoja:

- Elixirin virallinen dokumentaatio: [https://elixir-lang.org/docs.html](https://elixir-lang.org/docs.html)
- HTTPoison-kirjaston Github-sivu: [https://github.com/edgurgel/httpoison](https://github.com/edgurgel/httpoison)