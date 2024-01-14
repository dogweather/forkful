---
title:                "Fish Shell: Pisanie do standardowego błędu"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Praca z błędami w programowaniu może wydawać się frustrująca, ale często jest niezbędna do poprawnego działania aplikacji. Pisanie do standardowego błędu może pomóc w odnajdywaniu i rozwiązywaniu problemów, dzięki czemu kod będzie działać bardziej efektywnie.

## Jak to zrobić

Aby pisać do standardowego błędu w Fish Shell, możemy użyć polecenia `echo` z opcją `&2` oraz podać wiadomość lub zmienną, która ma zostać wyświetlona. Na przykład, jeśli chcemy wyświetlić wiadomość "Błąd wykonania skryptu" do standardowego błędu, możemy użyć następującego kodu:

```Fish Shell
echo "Błąd wykonania skryptu" &2
```

To spowoduje wyświetlenie tej wiadomości jako błędu w terminalu.

Dodatkowo, jeśli chcemy zapisywać błędy do pliku, możemy użyć "przekierowania strumienia", które pozwoli nam przekierować wynik `echo` do pliku tekstowego. Przykładowy kod wyglądałby tak:

```Fish Shell
echo "Błąd wykonania skryptu" &2 > error.log
```

Ten kod spowoduje zapisanie wiadomości "Błąd wykonania skryptu" do pliku error.log zamiast wyświetlania jej w terminalu.

## Głębsze zagadnienia

Pisanie do standardowego błędu może być także przydatne w debugowaniu aplikacji. Błędy, które nie są łatwo zauważalne w konsoli, mogą zostać zapisane do pliku, co ułatwi ich analizę i rozwiązanie.

Należy jednak pamiętać, że standardowy błąd jest połączony ze standardowym wyjściem, dlatego warto korzystać z funkcji `>&` do oddzielenia tych dwóch strumieni. Na przykład, jeśli chcemy zapisać tylko błędy do pliku, a wyświetlać wszystko inne w terminalu, możemy użyć tego kodu:

```Fish Shell
&> log.txt echo "Błąd wykonania skryptu" &2
```

W tym przypadku, wiadomość o błędzie zostanie zapisana do pliku log.txt, a inne komunikaty będą wyświetlane w konsoli.

## Zobacz również

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/)
- [Podręcznik użytkownika Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Przekierowanie strumienia w Fish Shell](https://fishshell.com/docs/current/tutorial.html#redirection)