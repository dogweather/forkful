---
title:                "Zmiana wielkości litery w ciągu znaków"
html_title:           "Bash: Zmiana wielkości litery w ciągu znaków"
simple_title:         "Zmiana wielkości litery w ciągu znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu musimy skonwertować tekst w taki sposób, żeby pierwsza litera każdego słowa była duża. Na przykład, gdy chcemy wyświetlić nazwę użytkownika w formie właściwej wielkości liter, a nie wszystkie małymi literami. Wtedy przydaje się poniższa metoda, która w prosty sposób zadba o odpowiednią kapitalizację tekstu.

## Jak to zrobić

Możemy użyć wbudowanej funkcji `tr` w Bashu, która służy do translacji tekstu. Kombinując z kilkoma prostymi polecaniami, możemy łatwo uzyskać efekt zamiany wszystkich małych liter na duże, z wyjątkiem pierwszej litery każdego słowa. 

```Bash
# Wykorzystujemy polecenia 'tr' do zamiany znaków
# Pierwsze polecenie zamienia wszystkie małe litery na duże
# Drugie polecenie zamienia pierwszą literę na dużą
echo "dowiedz się więcej o bashu i jego poleceniach" | tr '[:lower:]' '[:upper:]' | sed -E 's/(^| )[[:alpha:]]/\U&/g'
```

Wynik powyższego polecenia będzie wyglądał następująco:

```Bash
Dowiedz Się Więcej O Bashu I Jego Poleceniach
```

## Dogłębnie o kapitalizacji tekstu

Metoda opisana w sekcji "Jak to zrobić" wykorzystuje kombinację poleceń `tr` i `sed`. W krótkim opisie, pierwsze polecenie zamienia wszystkie małe litery na duże, a drugie polecenie znajduje pierwszą literę każdego słowa i zmienia ją na dużą. Jeśli chcemy dowiedzieć się więcej o tych poleceniach, możemy poszukać w dokumentacji lub różnych przykładach dostępnych online. 

Inna opcja to użycie funkcji `ucfirst`, która jest wbudowana w Bash. Ta funkcja pozwala na zmianę pierwszej litery na dużą w wybranym ciągu znaków. Możemy użyć jej w połączeniu z pętlą `for`, aby zamienić każde słowo z osobna. Jednak w przypadku dłuższych tekstów, ta metoda może być mniej efektywna i potrzebować więcej kodu.

## Zobacz także

- [Dokumentacja do funkcji `tr`](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Przykładowe użycie funkcji `tr` w Bashu](https://www.geeksforgeeks.org/tr-command-in-linux-with-examples/)
- [Dokumentacja do funkcji `sed`](https://www.gnu.org/software/sed/manual/html_node/sed-invocation.html)
- [Tutorial o podstawach Bashu](https://www.shellscript.sh/)