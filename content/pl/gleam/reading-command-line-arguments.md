---
title:                "Gleam: Odczytywanie argumentów linii poleceń"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O co chodzi?

Jeśli jesteś programistą lub chcesz się nauczyć programowania, na pewno słyszałeś o języku programowania Gleam. Jest to nowoczesny i wydajny język funkcyjny, który zyskuje coraz większą popularność wśród programistów. Dziś przyjrzymy się jednemu z podstawowych aspektów programowania w Gleam - odczytywaniu argumentów wiersza poleceń.

## Jak to zrobić?

Odczytywanie argumentów wiersza poleceń w języku Gleam jest bardzo proste. Wystarczy użyć funkcji o nazwie `gleam/gleam:command_line:args`, która zwraca listę wszystkich argumentów przekazanych do programu. Przykładowo, jeśli chcemy odczytać dwa argumenty przekazane do programu, musimy wywołać funkcję w następujący sposób:

```
Gleam import gleam/gleam

fn main() {
  let args = gleam/gleam:command_line:args()
  println("Pierwszy argument: #{args[0]}")
  println("Drugi argument: #{args[1]}")
}
```

W powyższym przykładzie użyliśmy funkcji `println`, aby wyświetlić odczytane argumenty wiersza poleceń. Możemy również użyć pętli `for` w celu przejścia przez wszystkie argumenty przekazane do programu.

## Deep Dive

Podczas odczytywania argumentów wiersza poleceń, należy pamiętać o kilku ważnych rzeczach. Po pierwsze, `gleam/gleam:command_line:args` zwraca listę typu `List(String)`, co oznacza, że możemy używać standardowych funkcji listowych, takich jak `length` czy `map`.

Po drugie, warto pamiętać o obsłudze błędów. Jeśli użytkownik nie przekaże odpowiedniej liczby argumentów, nasz program może zakończyć się niepowodzeniem. Dlatego warto wykorzystać mechanizm obsługi błędów w języku Gleam, aby przewidzieć takie sytuacje i odpowiednio zareagować.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o programowaniu w języku Gleam, warto przejrzeć inne artykuły na naszym blogu lub zapoznać się z dokumentacją języka. Poniżej znajdują się kilka przydatnych linków:

- [Oficjalna strona języka Gleam](https://gleam.run/)
- [Dokumentacja języka Gleam](https://gleam.run/documentation/)
- [Artykuły na temat Gleam na Medium](https://medium.com/tag/gleam)

Dziękujemy za przeczytanie tego artykułu i życzymy powodzenia w nauce programowania w języku Gleam!