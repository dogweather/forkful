---
title:                "Wyszukiwanie i zamienianie tekstu"
html_title:           "Bash: Wyszukiwanie i zamienianie tekstu"
simple_title:         "Wyszukiwanie i zamienianie tekstu"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami musimy przeprowadzić zmiany w wielu plikach jednocześnie, takich jak zmiana nazwy pliku lub korekta ortograficzna. W takich przypadkach używanie narzędzi do wyszukiwania i zamiany tekstu jest bardzo przydatne i może zaoszczędzić dużo czasu i wysiłku.

## Jak to zrobić

Aby wyszukać i zamienić tekst w plikach za pomocą Bash, wystarczy użyć polecenia `sed` (stream editor). Przykładowe użycie wygląda następująco:

```Bash
sed 's/stary_tekst/nowy_tekst/g' plik.txt
```

Powyższe polecenie wyszukuje wszystkie wystąpienia tekstu "stary_tekst" w pliku "plik.txt" i zamienia je na "nowy_tekst". Opcja `-i` może być użyta w celu bezpośredniej edycji pliku, bez konieczności wyświetlania zmian na ekranie.

## Wnikliwa analiza

Polecenie `sed` daje nam dużą kontrolę nad sposobem wyszukiwania i zamiany tekstu. Możemy na przykład ograniczyć zmiany tylko do konkretnych linii za pomocą opcji `-n` i wyrażenia regularnego. Możemy również dowolnie manipulować wybranymi fragmentami tekstu, np. zamieniać kolejność słów lub znaków.

Jeśli chcielibyśmy wykonać wiele różnych zmian w jednym pliku, pomocne może być użycie pliku z listą poleceń. Wystarczy utworzyć plik z rozszerzeniem `.sed` zawierający listę poleceń takich jak w przykładzie poniżej:

```Bash
s/stary_tekst/nowy_tekst/g
s/dawna_nazwa/nowa_nazwa/g
```

Następnie uruchamiamy polecenie `sed` z opcją `-f` i podajemy jako argument nasz plik z listą poleceń.

To tylko kilka przykładów możliwości, jakie daje nam narzędzie `sed` w wyszukiwaniu i zamianie tekstu. W celu pogłębienia swojej wiedzy, warto zapoznać się ze szerszymi możliwościami i opcjami tego polecenia.

## Zobacz również

- [Dokumentacja sed](https://www.gnu.org/software/sed/manual/)
- [Podręcznik Bash](https://www.gnu.org/software/bash/manual/)
- [Wprowadzenie do wyrażeń regularnych w Bash](https://www.digitalocean.com/community/tutorials/an-introduction-to-regular-expressions-in-bash)