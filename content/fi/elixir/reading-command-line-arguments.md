---
title:    "Elixir: Komentoriviparametrien lukeminen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Miksi

Komentoriviparametrien lukeminen on tärkeä osa Elixir ohjelmointia. Se antaa sinulle mahdollisuuden kommunikoida ja vaihtaa tietoja ohjelman ja käyttäjän välillä. Tämä tekee ohjelmasta helposti muokattavan ja skaalautuvan.

# Miten tehdä

Ensimmäinen askel komentoriviparametrien lukemisessa on määrittää muuttujat, joissa haluat tallentaa parametrit.

```Elixir
args = System.argv
```

Tämä koodi tallentaa kaikki komentoriviltä annetut parametrit listaan nimeltä ```args```. Voit myös määrittää tietyn parametrin indeksin perusteella tai käyttää avainsanaa ```value``` saadaksesi parametrin arvon.

Seuraavassa esimerkissä tulostamme toisen komentoriviparametrin:

```Elixir
second_arg = System.argv[2]

IO.puts "The second argument is: #{second_arg}"
```

Kun suoritat tämän koodin antamalla komentoriville esimerkiksi seuraavan komennon: ```elixir read_args.exs --example arg1 arg2 arg3```

Tulos olisi seuraava:

```
The second argument is: arg2
```

# Syvällinen sukellus

Voit myös käyttää kirjastoa nimeltä ```Clap``` helpottamaan komentoriviparametrien lukemista. Tämä kirjasto tarjoaa lisää toimintoja, kuten automaattisen virheenkäsittelyn, parametriohjeet ja muita edistyneitä ominaisuuksia.

## Määrittele parametrit

Voit määrittää tarvittavat parametrit käyttämällä ```Clap``` kirjaston funktiota ```parse```. Tässä esimerkissä määritämme parametrin nimeltä ```help``` ja sille asetamme oletusarvoksi ```false```:

```Elixir
opts = Clap.parse(
  [
    version: "1.0.0",
    help: false
  ]
)
```

## Käsittele virheet

Jos käyttäjä ei anna tarvittavaa parametria, ```Clap``` automaattisesti tulostaa virheilmoituksen ja näyttää mahdolliset parametrit, jotka käyttäjä voi antaa.

## Näytä ohjeet

Voit myös lisätä ohjeita parametreille käyttämällä funktiota ```help```. Voit määrittää parametrille haluamasi nimet ja lisätä lyhyt kuvaus toiminnosta parametrille.

```Elixir
opts = Clap.parse(
  [
    version: "1.0.0",
    help: [
      short: "h",
      desc: "Show help"
    ]
  ]
)
```

# Katso myös

- [Elixir Command Line API](https://hexdocs.pm/elixir/System.html)
- [Clap library documentation](https://hexdocs.pm/clap/)
- [Elixir Command Line Applications tutorial](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#creating-command-line-applications)