---
title:                "Bash: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest nieodłączną częścią wielu programów i skryptów Bash. Dzięki temu funkcjonalności możemy uzyskać różnorodność i zaskoczenie w naszych działaniach. Czytaj dalej, aby dowiedzieć się, jak to zrobić!

## Jak

Istnieje kilka sposobów na generowanie losowych liczb w Bash. Jednym z najprostszych jest użycie polecenia `shuf` wraz z opcją `-i` oraz podaniem zakresu liczb, z którego chcemy wylosować. Przykładowo:

```Bash
shuf -i 1-10
```

Wygeneruje losową liczbę z przedziału od 1 do 10. Możemy również użyć polecenia `jot`, które pozwala na wygenerowanie większej ilości losowych liczb, na przykład:

```Bash
jot -r 5 1-100
```

Wygeneruje 5 losowych liczb z przedziału od 1 do 100.

Inną metodą jest użycie pętli `for` w połączeniu z wyrzuceniem wygenerowanych liczb do pliku, na przykład:

```Bash
for i in {1..10}; do
  echo $RANDOM >> numbers.txt
done
```

Spowoduje wygenerowanie 10 losowych liczb i zapisanie ich do pliku numbers.txt. Istnieje wiele innych sposobów na generowanie losowych liczb w Bash, więc warto zobaczyć, który z nich będzie najbardziej odpowiedni dla naszego konkretnego przypadku.

## Deep Dive

Warto wspomnieć, że użycie polecenia `RANDOM` w Bash również pozwala na generowanie losowych liczb, jednakże jest to bardziej ograniczone i działa tylko w przedziale od 0 do 32767. Istnieją również bardziej zaawansowane metody, takie jak użycie funkcji `mktemp` lub wykorzystanie połączenia zewnętrznych bibliotek do generowania liczb pseudolosowych.

## Zobacz również

- [Kurs Bash w języku polskim](https://pl.wikibooks.org/wiki/Bash)
- [Oficjalna dokumentacja Bash](https://www.gnu.org/software/bash/manual/)
- [Przewodnik po poleceniu `shuf` w Bash](https://www.tutorialspoint.com/unix_commands/shuf.htm)
- [Więcej informacji o generowaniu losowych liczb w Bash](https://www.cyberciti.biz/faq/unix-linux-generating-random-passwords/)

Dzięki tym informacjom, generowanie losowych liczb w Bash powinno już nie być dla Ciebie problemem! Sprawdź również inne sposoby, aby dostosować wygenerowane liczby do swoich potrzeb i śmiało wykorzystuj je w swoich skryptach i programach.