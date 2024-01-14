---
title:    "Bash: Znajdowanie długości ciągu znaków"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego
 
 Język Bash jest powszechnie używany do wykonywania różnych zadań związanych z administracją systemem operacyjnym. Jedną z podstawowych operacji jest znajdowanie długości ciągu znaków. W tym poście dowiesz się, dlaczego to ważne i jak łatwo znaleźć długość ciągu w Bash.

## Jak To Zrobić

Aby znaleźć długość ciągu w Bash, możesz skorzystać z wbudowanej funkcji `length`, która zwraca długość przekazanego ciągu. Spróbujmy wyświetlić długość słowa "programowanie" za pomocą polecenia `echo`:

```Bash
echo ${#programowanie}
```

Wynik powinien wyglądać następująco: `13`.

W przypadku zmiennej zawierającej ciąg znaków, możesz również wykorzystać polecenie `expr length`, które zwraca jego długość. Przykładowo:

```Bash
zmienna="Hello, World!"
echo $(expr length $zmienna)
```

Ten kod wyświetli liczbę 13, ponieważ tyle znaków znajduje się w zmiennej `zmienna`.

## Deep Dive

Zarówno funkcja `length` jak i polecenie `expr length` zwracają długość ciągu łącznie z białymi znakami. Jeśli chcesz wyświetlić tylko liczbę znaków bez spacji, musisz wykorzystać dodatkową funkcję `wc` i jej przełącznik `-w`, który zlicza tylko słowa. Przykładowo, jeśli chcesz wyświetlić liczbę znaków w ciągu "Hello, World!" bez spacji, musisz użyć następującego kodu:

```Bash
zmienna="Hello, World!"
echo $(expr length $zmienna | wc -w)
```

Wynik powinien zwrócić tylko liczbę 11, ponieważ zliczane są tylko posiada 11 słów, bez spacji.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o wykorzystywaniu języka Bash w codziennej pracy lub do nauki programowania, zapoznaj się z poniższymi linkami:

 - [Oficjalna dokumentacja języka Bash](https://www.gnu.org/software/bash/manual/bash.html)
 - [Podstawy programowania w Bash na przykładach](http://mischasan.wordpress.com/category/GNU+Linux/software/Bash/)
 - [Bash Academy - materiały do nauki Bash w Języku Polskim](http://www.bash.academy/pl/index.html)