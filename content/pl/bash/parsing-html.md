---
title:                "Analizowanie html"
html_title:           "Bash: Analizowanie html"
simple_title:         "Analizowanie html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/parsing-html.md"
---

{{< edit_this_page >}}

## O co chodzi?
 Parsowanie HTML to proces odczytywania i interpretowania kodu HTML, który określa strukturę i zawartość strony internetowej. Programiści wykorzystują to narzędzie, aby dostosować lub wyświetlić wybrane elementy na stronie, w celu lepszego rozmieszczenia i wyglądu.

## Jak to zrobić:
Poniżej znajdziesz dwa przykłady kodu w Bash, które pokażą Ci, jak podstawowe parsowanie HTML jest realizowane w tym języku programowania:

- Przykład 1: Pobranie tytułu strony internetowej za pomocą polecenia ```curl``` i ```grep```
```
$ curl -s example.com | grep "<title>.*</title>" | sed -E 's/<\/?title>//g'
```
Wynik:
```
Example Domain
```
- Przykład 2: Wypisanie wszystkich linków ze strony internetowej za pomocą polecenia ```lynx```
```
$ lynx -dump -listonly example.com
```
Wynik:
```
http://example.com
https://www.iana.org/domains/example
http://www.iana.org/domains/example
```

## Wprowadzenie w temat:
Parsowanie HTML jest istotną częścią tworzenia stron internetowych od czasów ich początku. Początkowo wykorzystywano to narzędzie, aby odczytać treść strony i pozyskać informację ze stron internetowych. W dzisiejszych czasach, programiści wykorzystują parsowanie HTML do automatyzacji procesów związanych z analizowaniem struktury i zawartości stron internetowych. Alternatywą dla Bash są języki specjalizujące się w parsowaniu i manipulowaniu tekstu, takie jak Perl, Python czy Ruby. W Bash używa się poleceń takich jak ```grep```, ```sed``` i ```awk``` do parsowania HTML.

## Zobacz także:
Jeśli chcesz dowiedzieć się więcej o parsowaniu HTML w Bash, polecamy zapoznać się z poniższymi źródłami:
- [Oficjalna strona dokumentacji Bash](https://www.gnu.org/software/bash/) - oficjalna strona dokumentacji języka Bash.
- [Bash Guide for Beginners](http://www.tldp.org/LDP/Bash-Beginners-Guide/html/index.html) - kompletne wprowadzenie do języka Bash dla początkujących.
- [Three Reasons to Learn Bash Scripting](https://linuxacademy.com/blog/linux/three-reasons-to-learn-bash-scripting/) - artykuł omawiający zalety nauki Bash.
- [Bash Scripting Tutorial for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners) - praktyczne instrukcje do nauki Bash.