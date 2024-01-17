---
title:                "Zapisywanie w wielkich literach ciągu znaków"
html_title:           "Fish Shell: Zapisywanie w wielkich literach ciągu znaków"
simple_title:         "Zapisywanie w wielkich literach ciągu znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

[
## Co i dlaczego?
Zdaje sobie sprawę, że działanie skryptów i kodów może nie być zbyt przyjemne dla oka, szczególnie jeśli nie są poprawnie zformatowane. Jedną z technik, aby ułatwić sobie czytanie kodu jest stosowanie kapitalizacji. Polega to na zmianie pierwszej litery tekstu na dużą, dzięki czemu od razu jest ona rozpoznawalna jako nazwa zmiennej lub klasa. Programiści stosują tę technikę, aby ich kod stał się bardziej czytelny i łatwiejszy w analizie.

## Jak to zrobić:
Fish Shell oferuje kilka metod na kapitalizację tekstu. Możesz użyć funkcji ```string capitalize```, która zmienia pierwszą literę wybranego ciągu znaków na dużą. Przykładowy kod wygląda następująco:

```
string txt = "hello world"
echo (string capitalize $txt)
```
Wynikiem będzie ```Hello world```.

Można również skorzystać z flagi ```-k```, aby kapitalizować nie tylko pierwszą, ale wszystkie litery w wybranym tekście. Na przykład:

```
echo (lscolor -k --owner $HOME)
```
Powyższy kod wyświetli listę plików w katalogu domowym, ale z nazwami wszystkich plików zapisanymi dużymi literami.

## Deep Dive:
Kapitalizacja jest powszechnie stosowaną techniką w programowaniu, która pomaga w czytaniu i analizie kodu. Jest też często wymagana w przypadku, gdy nazwa zmiennej lub funkcji jest złożona z wielu słów. Alternatywnymi sposobami kapitalizacji w Fish Shell są funkcje ```string upper``` i ```string title```. Pierwsza zmienia wszystkie litery w tekście na duże, a druga zmienia pierwszą literę każdego słowa w tekście na dużą. Implementacja tych funkcji jest oparta na standardowych programach Unixowych, co zapewnia efektywność i wydajność. 

## Zobacz też:
Jeśli interesujesz się programowaniem w Fish Shell, warto zapoznać się z funkcjami ```string lower```, ```string join``` oraz ```string match```. Możesz również przeczytać dokumentację na oficjalnej stronie Fish Shell, gdzie znajdziesz więcej przykładowych kodów i informacji na temat kapitalizacji i innych przydatnych funkcji.