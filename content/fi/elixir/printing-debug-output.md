---
title:                "Elixir: Tulostuksen virheenjäljitys"
simple_title:         "Tulostuksen virheenjäljitys"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Koodin debuggaaminen voi olla haastavaa, varsinkin kun yritetään löytää virheitä suuremmassa koodipohjassa. Elixir tarjoaa kuitenkin huiman kätevän tavan näyttää debug outputia ohjelman ajon aikana, joten miksi ei hyödyntäisi tätä mahdollisuutta?

## Miten tehdä se

Debug outputin näyttäminen Elixirissä on helppoa. Käytä yksinkertaisesti `IO.inspect()`-funktiota ja lisää sille argumentiksi haluamasi muuttuja tai arvo. Voit jopa antaa `IO.inspect()`-funktiolle useamman kuin yhden argumentin, mikä näyttää useamman arvon kerralla.

```Elixir
name = "Pekka"
age = 30
IO.inspect(name)
# Output: "Pekka"
IO.inspect(name, age)
# Output: "Pekka", 30
```

Voit myös tulostaa kompleksisempia rakenteita, kuten listoja tai mappeja. Käytä silloin `IO.inspect()`-funktiolle `:label`-argumenttina `:pretty`-arvoa, jotta tuloste on helposti luettavissa.

```Elixir
my_list = [1, 2, 3, 4]
IO.inspect(my_list, label: :pretty)
# Output: [1, 2, 3, 4]
```

## Syvempi sukellus

`IO.inspect()`-funktiolla on myös muita hyödyllisiä argumentteja, kuten `:color` ja `:char_lists`, jotka mahdollistavat värien ja merkkijonojen käytön tulosteessa.

Voit myös käyttää `IO.inspect()`-funktiota funktion sisällä, jolloin voit nähdä funktion suoritustiedot ja mahdolliset virheet helposti.

```Elixir
def my_function(arg1, arg2) do
  IO.inspect(arg1)
  IO.inspect(arg2)
  # Muita toimintoja
end
```

## Katso myös

- [Elixir Dokumentaatio - Debugging](https://elixir-lang.org/getting-started/debugging.html)
- [ElixirSchool - Debugging](https://elixirschool.com/en/lessons/advanced/debugging/)
- [Elixir Forum - IO.inspect() discussion](https://elixirforum.com/t/elixir-i-o-inspect-vs-pipes/2236)