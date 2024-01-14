---
title:                "Bash: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek musiałeś ręcznie zmieniać wszystkie wystąpienia konkretnego słowa w swoim pliku lub kodzie? To męczące zadanie, które może zająć dużo czasu. Ale nie martw się, jest sposób na szybsze i wydajniejsze zastąpienie tekstu - za pomocą programowania w Bashu.

## Jak to zrobić

Aby zastąpić tekst w pliku lub kodzie za pomocą Bashu, musisz użyć wbudowanej funkcji sed (stream editor). Najpierw masz do dyspozycji polecenie "s/tekst-do-zastapienia/nowy-tekst/", które zmienia pierwsze wystąpienie tekstu w danej linii. Jeśli chcesz zmienić wszystkie wystąpienia, musisz dodać "g" na końcu, na przykład "s/tekst-do-zastapienia/nowy-tekst/g". Możesz również dodać opcję "i" do pierwszej litery polecenia, aby zignorować wielkość liter, na przykład "s/tekst-do-zastapienia/nowy-tekst/gi". Oto przykładowy kod w Bashu przed i po użyciu sed:

```Bash
# Przed użyciem sed
echo "Hello world!" > plik.txt
cat plik.txt
# Hello world!

# Po użyciu sed
sed -i 's/world!/Dziś jest piękny dzień!/g' plik.txt
cat plik.txt
# Hello Dziś jest piękny dzień!
```

Możesz również zastąpić tekst w wielu plikach naraz przez użycie polecenia find z opcją -exec. Na przykład, aby zastąpić "Hello" na "Cześć" we wszystkich plikach z rozszerzeniem .txt, możesz użyć następującego kodu:

```Bash
find . -name "*.txt" -exec sed -i 's/Hello/Cześć/g' {} \;
```

## Deep Dive

Aby lepiej zrozumieć jak działają polecenia s/.../.../ i find z opcją -exec, warto wiedzieć, że sed korzysta z tzw. wyrażeń regularnych (regular expressions). Jest to bardzo przydatne narzędzie do wyszukiwania tekstów z różnymi wzorcami, na przykład wszystkich liczb, słów zaczynających się na daną literę czy konkretnych znaków. Istnieje wiele różnych wyrażeń regularnych, więc warto poszukać ich listy i zaznajomić się z tym narzędziem, aby wykorzystać go w pełni.

Polecenie find natomiast służy do wyszukiwania plików zgodnych z określonymi kryteriami, takimi jak rozszerzenie, nazwa czy data modyfikacji. Opcja -exec pozwala uruchomić polecenie dla każdego odnalezionego pliku, w tym przypadku sed. Jest to więc bardzo wygodny sposób na automatyczne zastąpienie tekstu w wielu plikach naraz.

Pamiętaj również, że sed i find nie są dostępne tylko w Bashu - są to narzędzia systemowe, które są również dostępne w innych językach programowania i systemach operacyjnych.

## Zobacz również

- [Wprowadzenie do sed na stronie Linux Academy (w języku angielskim)](https://linuxacademy.com/guide/6058-grep-sed-and-awk/)
- [Lista wyrażeń regularnych w Wikipedii (w języku polskim)](https://pl.wikipedia.org/wiki/Wyrażenie_regularne)
- [Dokumentacja do find w manualu systemowym (w języku angielskim)](http://man7.org/linux/man-pages/man1/find.1.html)