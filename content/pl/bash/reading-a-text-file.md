---
title:    "Bash: Odczytywanie pliku tekstowego"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek chciałeś/łaś przeczytać duży plik tekstowy w systemie Linux i mieć dostęp do jego zawartości z poziomu terminala? Może potrzebujesz szybko znaleźć specyficzne informacje, ale masz problem z przeszukiwaniem ogromnych ilości tekstu? W tym tekście przeczytasz o tym, jak w Bashu możesz szybko i łatwo odczytać plik tekstowy i wykorzystać go do swoich potrzeb.

## Jak to zrobić

Aby odczytać plik tekstowy w Bashu, wykorzystujemy komendę `cat` (od *concatenate*) wraz z odpowiednią ścieżką do pliku. Przykładowo, jeśli chcemy odczytać plik tekstowy o nazwie `tekst.txt`, będący w tym samym folderze co aktualny terminal, wpisujemy `cat tekst.txt`. W ten sposób zostanie wyświetlona cała zawartość pliku w terminalu.

```Bash
cat tekst.txt
```

Aby wyświetlić jedynie określoną ilość linii z pliku tekstowego, możemy wykorzystać parametr `-n` oraz liczbę określającą ilość linii do wyświetlenia. Na przykład, jeśli chcemy wyświetlić pierwsze 10 linii pliku, wpisujemy `cat -n 10 tekst.txt`. W podobny sposób możemy wyświetlić ostatnie linie pliku, wykorzystując parametr `-n -10`.

```Bash
cat -n 10 tekst.txt
```

Możemy również użyć komendy `head` oraz `tail`, aby wyświetlić odpowiednio pierwsze i ostatnie linie pliku tekstowego. Dzięki temu unikamy konieczności podawania numerów linii.

```Bash
head tekst.txt
```

```Bash
tail tekst.txt
```

Jeśli chcemy dokonać bardziej zaawansowanej manipulacji tekstem z pliku, na przykład wyświetlić jedynie linie zawierające określone słowo, możemy skorzystać z potężnej komendy `grep`. Przykładowo, jeśli chcemy wyświetlić jedynie linie zawierające słowo "Bash", wpisujemy `cat tekst.txt | grep Bash`. W ten sposób zostaną wyświetlone tylko linie, które zawierają słowo "Bash".

```Bash
cat tekst.txt | grep Bash
```

## Deep Dive

Chociaż wyświetlanie zawartości pliku tekstowego w terminalu jest przydatne, dobrze jest znać kilka przydatnych opcji, aby upewnić się, że odczytujemy tylko potrzebne nam informacje. Przykładowo, jeśli chcemy zignorować dziwne znaki ascii, które czasami mogą znajdować się w tekście, możemy skorzystać z parametru `-v` w komendzie `grep`. W ten sposób zostaną wyświetlone tylko linie, które nie zawierają tych znaków.

```Bash
cat tekst.txt | grep -v 'ascii'
```

Inną przydatną opcją jest `grep -i`, która ignoruje wielkość liter w poszukiwanym słowie. W ten sposób możemy łatwiej znaleźć wszystkie linie zawierające słowo, niezależnie od tego, czy jest ono napisane wielkimi czy małymi literami.

```Bash
cat tekst.txt | grep -i 'BASH'
```

## Zobacz także

- [Podstawy środowiska bash w Linuxie](https://linux.pl/kurs_bash)
- [7 przydatnych trików w Bashu](https://www.shellscript.sh/tips/)
- [Dokumentacja komendy `cat`](https://ss64.com/bash/cat.html)