---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Elm: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Cześć programiści i programistki! Dziś chciałbym podzielić się z wami informacjami na temat konwertowania stringów na małe litery w języku Elm. Na początku, nie jesteście przypadkiem zainteresowani czym tak naprawdę jest to konwertowanie i dlaczego jest to ważne w programowaniu? Jeśli tak, to zapraszam do lektury!

## Co & Dlaczego?
Konwertowanie stringów na małe litery to nic innego jak zmiana wszystkich wielkich liter na odpowiadające im małe litery. Często wykonujemy tę operację na danych, które otrzymujemy od użytkownika lub z zewnętrznych źródeł, aby uniknąć różnic w pisowni i ułatwić manipulację tekstem. Dzięki temu nie musimy przyjmować kilku różnych wersji tej samej informacji i możemy spokojnie przetwarzać tekst w naszym programie.

## Jak to zrobić?
Elm ma wbudowaną funkcję o nazwie `String.toLower`, która pozwala nam konwertować stringi na małe litery. Oto przykład jej użycia:
```Elm
> String.toLower "Witaj Świecie"
"witaj świecie"
```
Proste, prawda? Jeśli chcemy wyświetlić rezultat w naszym programie, możemy zastosować tę funkcję w naszym wyrażeniu:
```Elm
showResult : String -> Html Msg
showResult str =
    div [] [ text (String.toLower str) ]
```
Teraz każdy tekst podany do `showResult` zostanie przekonwertowany na małe litery i wyświetlony na stronie.

## Głębsza Analiza
Wcześniej wspomniałem, że konwersja stringów na małe litery jest często stosowana, aby ułatwić manipulację tekstem. Jednak warto też wiedzieć, że istnieją inne rozwiązania takie jak `String.toUpper` (konwertujące na wielkie litery) czy `String.toTitle` (zmieniające pierwszą literę na wielką). W przypadku gdy potrzebujemy bardziej zaawansowanego przetwarzania tekstu, możemy skorzystać z modułu `String.Extra`, który oferuje szereg funkcji do operowania na stringach.

## Zobacz też
Jeśli chcesz dowiedzieć się więcej na temat funkcji `String.toLower` i jej użycia, zapraszam do odwiedzenia tej [strony](https://package.elm-lang.org/packages/elm-lang/core/3.0.0/String#toLower) w dokumentacji Elm. Jeśli potrzebujesz pomocy w innych kwestiach związanych z programowaniem w Elm, polecam [Elm Poland](https://elm.pl/) - polskie forum dla miłośników tego języka!

Do zobaczenia w kolejnym artykule!