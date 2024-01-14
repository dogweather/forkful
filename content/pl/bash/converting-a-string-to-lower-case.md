---
title:                "Bash: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie ciągu znaków na małe litery jest często wykorzystywane w programowaniu, aby ułatwić porównywanie i manipulowanie tekstem. Jest to szczególnie przydatne, gdy pracujesz z danymi pobranymi z zewnętrznych źródeł, które mogą mieć różne wielkości liter lub pomiędzy systemami o różnych ustawieniach lokalnych.

## Jak To Zrobić

Aby zamienić ciąg znaków na małe litery w Bash, można użyć wbudowanej funkcji "tr" w poniższym kodzie:

```Bash
string="PRZYKŁADOWY TEKST"
echo "$string" | tr '[:upper:]' '[:lower:]'
```

Jego wyjście będzie następujące:

```Bash
przykładowy tekst
```

Kod powyżej używa funkcji "echo" do wyświetlenia oryginalnego tekstu, a następnie przesyła wynik do funkcji "tr" wraz z dwoma zestawami znaków. Pierwszy zestaw oznacza wszystkie duże litery, a drugi zestaw oznacza wszystkie małe litery. W ten sposób wszystkie duże litery są zamieniane na małe litery w ciągu.

Można również wykorzystać pętlę "for" do zamiany na małe litery każdego pojedynczego wyrazu w tekście, jak w poniższym przykładzie:

```Bash
string="PRZYKŁADOWY TEKST"
for word in $string; do
    echo "${word,,}"
done
```

Jego wyjście będzie następujące:

```Bash
przykładowy 
tekst
```

W powyższym kodzie zmienna "word" reprezentuje każde pojedyncze słowo w tekście, a operator "${}" pozwala na zamianę na małe litery.

## Głębsza Analiza

Istnieje wiele różnych sposobów, aby zamienić ciąg znaków na małe litery w Bash, włączając wiele funkcji takich jak "sed", "awk" i "perl". Każda z nich ma swoje własne zalety i jest używana w różnych przypadkach w zależności od potrzeb programisty.

Jedną z popularnych metod jest użycie funkcji "tr" wraz z parametrem "-c" do wykluczenia specyficznych znaków z zamiany. Na przykład, jeśli chcesz zachować oryginalną wielkość znaków nawiasów, możesz użyć poniższego kodu:

```Bash
string="Przykładowy TEKST (ZS) Wielkiego miasta."
echo "$string" | tr '[:upper:]' '[:lower:]' | tr '()' '()'
```

Jego wyjście będzie następujące:

```Bash
przykładowy tekst (ZS) wielkiego miasta.
```

Warto również pamiętać, że niektóre parametry transkrypcji mogą różnić się w zależności od ustawień regionalnych i systemowych. Dlatego zaleca się, aby wcześniej przetestować kod na różnych platformach.

## Zobacz również

- [Bash Scripting Tutorial for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [The Linux Command Line](https://linuxcommand.org/tlcl.php)
- [AWK - A Tutorial and Introduction](https://www.grymoire.com/Unix/Awk.html)