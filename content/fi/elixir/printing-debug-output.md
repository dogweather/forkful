---
title:    "Elixir: Ohjelman debuggaustulostuksen tulostaminen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi

Debuggaus on olennainen osa ohjelmoinnin prosessia, ja debug outputin tulostaminen on yksi tapa helpottaa virheiden ja ongelmien havaitsemista ja korjaamista. Se on myös hyödyllinen työkalu oppimisessa ja koodin tehokkuuden parantamisessa.

## Miten

Debug outputin tulostaminen Elixirillä on helppoa. Voit käyttää esimerkiksi `IO.puts()`-funktiota, joka tulostaa annetun arvon konsoliin. Voit myös käyttää `IO.inspect()`-funktiota, joka tulostaa annetun arvon lisäksi myös sen tietotyypin ja sijainnin muistissa.

```
Elixir
IO.puts("Tulosta tämä")
# Tulosta tämä

IO.inspect(5)
# 5 (Integer)

IO.inspect([1, 2, 3])
# [1, 2, 3] (List)
```

Voit myös käyttää `Logger`-moduulia, joka tarjoaa enemmän vaihtoehtoja debug outputin tulostamiseen esimerkiksi tiedostoihin tai tietokantoihin.

```
Elixir
Logger.debug("Tulostetaan tämä viesti debug-tilassa")
```

Vietetään myös huomiota siihen, että debug outputin tulostaminen voi vaikuttaa suorituskykyyn, joten sitä tulisi käyttää harkiten ja vain silloin kun sitä todella tarvitaan.

## Syvemmälle

Debug outputin tulostamisen käytännöt ja säännöt voivat vaihdella eri tilanteissa ja ohjelmointiprojekteissa. Joissakin tilanteissa voi olla hyödyllistä käyttää Elixirin hankinnan käsittelijöitä (`Procovers`), jotka mahdollistavat dynaamisen debug outputin tulostamisen tietyissä kohdissa koodia.

On myös tärkeää muistaa, että debug outputin tulostamista ei tulisi käyttää ainoana tapana virheiden havaitsemiseen ja korjaamiseen. Hyvä ohjelmointikäytäntö on myös yksikkötestien kirjoittaminen ja virheiden hallinnan strategioiden luominen.

## Katso myös

- [Virheiden hallinta Elixirillä](https://elixir-lang.org/getting-started/try-catch-and-rescue.html)
- [Yksikkötestaaminen Elixirillä](https://elixir-lang.org/getting-started/introduction-to-mix.html#automatically-generated-tests)