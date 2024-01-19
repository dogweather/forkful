---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Generowanie losowych liczb to proces tworzenia ciągu liczb, które nie mają widocznego wzoru i wydają się niezależne. Programiści robią to, żeby wprowadzać element niemożliwy do przewidzenia w kodzie, co jest kluczowe w grach, symulacjach i wielu innych zastosowaniach.

## Jak to zrobić:
Teraz napiszemy prosty kod w Gleam do generowania losowej liczby. Niech będzie to:

```Gleam
import gleam/number
import gleam/io.{println}
import rand/rand

pub fn main(args: List(String)) ->
  Nil
{
  let _ = io.println(toString(rand.int(0, 100))) 
}
```
Dla tego kodu wyjściem może być dowolna liczba od 0 do 100.

## Głębokie zrozumienie:
Generowanie liczb losowych ma długą historię, począwszy od starożytnych cywilizacji używających kości do gier. W kontekście samo Gleam, wtórna biblioteka `rand` jest powszechnie używana. Inne biblioteki, takie jak `random` i `random_seed`, są również dostępne, ale pierwszy jest preferowany ze względu na jego wydajność. Generowanie losowych liczb w Gleam zwraca nam Int, co oznacza, że będzie musiał konwertować tę liczbę na inny typ danych, jeżeli chce używać jej do czegoś innego.

## Zobacz także:
Jeżeli chcesz dowiedzieć się więcej o generowaniu losowych liczb, zasoby te mogą okazać się pomocne. 

[Gleam Documentation](https://hexdocs.pm/gleam_stdlib/gleam/number/)
[Random Number Generation in Programming](https://en.wikipedia.org/wiki/Random_number_generation)

Pamiętaj, że komunikacja jest kluczem do efektywnego kodowania. Więc nie obawiaj się pytać pytania, jak tylko nadejdą!