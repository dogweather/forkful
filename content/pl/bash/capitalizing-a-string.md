---
title:    "Bash: Formatowanie napisu"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

Witaj w naszym pierwszym wpisie na blogu poświęconym programowaniu w Bashu! Dzisiejszym tematem będzie zmiana pierwszej litery w zdaniu na wielką. Możesz się zastanawiać, po co w ogóle musielibyśmy zmieniać wielkość liter w tekście? Otóż, może się to przydać podczas pisania skryptów, gdzie chcemy aby wyświetlana informacja była bardziej czytelna lub po prostu podkreślić jakieś ważne słowo w zdaniu.

## Jak to zrobić

Aby zmienić pierwszą literę zdania na wielką, możemy skorzystać z wbudowanej funkcji `tr` w Bashu. Spójrzmy na przykładowy kod poniżej:

```bash
sentence="to jest przykładowe zdanie"
capitalized_sentence=$(echo "$sentence" | tr '[:lower:]' '[:upper:]')
echo $capitalized_sentence
```

W powyższym kodzie tworzymy zmienną `sentence`, która zawiera nasze zdanie. Następnie przy użyciu funkcji `echo` i `tr` tworzymy nową zmienną `capitalized_sentence`, która zawiera to samo zdanie, ale z pierwszą literą zmienioną na wielką. W końcu wyświetlamy nasze nowe zdanie za pomocą funkcji `echo`.

Po uruchomieniu tego kodu, powinniśmy zobaczyć wyjście `To jest przykładowe zdanie`, gdzie pierwsza litera została zmieniona na wielką. Proste, prawda?

## Głębsza analiza

Chcesz wiedzieć, w jaki sposób dokładnie działa funkcja `tr` i dlaczego musimy podać argument `[:lower:]` i `[:upper:]`? Otóż, `tr` jest używane do transliteryzacji, czyli zamiany jednego zestawu znaków na inny. W naszym przypadku, `tr` zamienia wszystkie znaki w pierwszym argumencie (tutaj `[:lower:]` - wszystkie małe litery) na odpowiadające im znaki w drugim argumencie (tutaj `[:upper:]` - wszystkie wielkie litery).

Ciekawostką jest fakt, że funkcja `tr` nie tylko działa na literach, ale także na innych zestawach znaków. Możemy na przykład zmienić wszystkie cyfry w tekście na gwiazdki, korzystając z `tr -s '[:digit:]' '*'`.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o funkcji `tr` lub o innych przydatnych funkcjach w Bashu, polecamy poniższe linki:

- https://www.linuxtechi.com/change-case-string-using-tr-command-linux/
- https://www.geeksforgeeks.org/tr-command-in-linux-with-examples/
- https://www.shell-tips.com/bash/math-arithmetic-calculation-in-bash/

Dziękujemy za przeczytanie naszego wpisu na temat zmieniania wielkości liter w Bashu. Mamy nadzieję, że ten mały trik okaże się dla Ciebie przydatny w przyszłych projektach!