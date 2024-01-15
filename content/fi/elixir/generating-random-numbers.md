---
title:                "Satunnaislukujen luominen"
html_title:           "Elixir: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi käyttää satunnaislukugeneraattoria? Satunnaisluvut ovat hyödyllisiä simulaatioissa, salaussovelluksissa, pelien ja arpajaisten piirtämisessä ja yleensä kaikissa tilanteissa, joissa tarvitaan sattumanvaraista arvoa.

## Miten

Globaalin satunnaislukugeneraattorin käyttö Elixirissä on helppoa. Käytämme siihen sisäänrakennettua :random -moduulia ja sen funktiota seed/3. Seuraavassa esimerkissä luomme 10 satunnaislukua välillä 1-100 ja tulostamme ne konsoliin:

```elixir
for _ <- 1..10 do
  random = :random.seed(:erlang.now, :os.timestamp, :rand.uniform)
  IO.inspect(:rand.uniform(1..100, random))
end
```

Tämä koodi käyttää nykyistä aikaleimaa, järjestelmän aikaleimaa ja satunnaista numeerista siementä luomaan kunkin satunnaisluvun. Tätä kutsutaan myös pseudosatunnaislukujen generoinniksi, koska luvut eivät ole täysin satunnaisia, mutta niillä on riittävästi sattumanvaraista elementtiä, jotta ne toimisivat useimmissa käyttötarkoituksissa.

## Syvällinen sukellus

Satunnaislukugeneraattorit voivat olla erittäin monimutkaisia ja niillä on omat sääntönsä. Esimerkiksi :random-moduuli käyttää Mersenne Twister -algoritmia, joka on kehitetty erityisesti satunnaislukujen generointia varten. Tämä algoritmi on suhteellisen tehokas ja tuottaa laadukkaita satunnaislukuja.

Hyvä tapa parantaa satunnaislukugeneraattorisi laatua on antaa sille erityinen siemenarvo, joka määrittää sen aloitustilan. Tämä estää samojen lukujen toistumisen, mikä voisi aiheuttaa ennakoitavissa olevia tuloksia. Voit myös käyttää muita funktioita, kuten :rand.uniform/1 ja :rand.uniform/2, jotka antavat sinulle mahdollisuuden määrittää tulosten tyyppi ja alue, jolta luvut generoidaan.

## Katso myös

- Elixirin virallinen dokumentaatio :random-moduulista (https://hexdocs.pm/elixir/random.html)
- "Real-Life Examples of the Mersenne Twister" (https://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/emt19937ar.html) - Esimerkkejä Mersenne Twister -algoritmista käytännön sovelluksissa