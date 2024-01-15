---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "C++: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy zdarzyło Ci się kiedyś korzystać z programów, które można uruchomić w terminalu i przekazać im pewne opcje lub argumenty? Jeśli tak, to prawdopodobnie spotkałeś się z koniecznością czytania argumentów z linii poleceń. W tym artykule dowiesz się, jak to zrobić przy pomocy języka C++, a także dlaczego jest to ważna umiejętność dla każdego programisty.

## Jak to zrobić

Dla jasności, przez "czytanie argumentów z linii poleceń" rozumiem odczytywanie wartości przekazanych do programu po jego uruchomieniu w terminalu. W języku C++ jest to możliwe dzięki zmiennej `argc`, będącej liczbą argumentów przekazanych do programu, oraz tablicy `argv`, zawierającej te argumenty.

Przykładowo, jeśli uruchomisz program nazwany "hello" w terminalu z argumentem "world", to odczytasz go w następujący sposób:

```C++
int main(int argc, char *argv[])
{
    std::cout << "Witaj " << argv[1] << "!" << std::endl;

    return 0;
}
```

W powyższym przykładzie `argv[1]` oznacza pierwszy argument przekazany do programu, który w tym przypadku jest równy "world". Oczywiście można przekazać więcej argumentów i odczytać je w podobny sposób.

## Deep Dive

W przypadku bardziej skomplikowanych programów, czytanie argumentów z linii poleceń może stać się nieco trudniejsze, ponieważ konieczne będzie dodatkowe przekształcanie stringów na odpowiednie typy danych. W takiej sytuacji przydatne mogą okazać się funkcje takie jak `std::atoi` lub `std::atof`, które pozwalają na konwersję stringów na liczby całkowite lub zmiennoprzecinkowe.

Kolejną ważną umiejętnością jest obsługa opcji uruchamiania programu, które mają postać np. "-f plik.txt". W tym wypadku konieczne jest przeszukiwanie tablicy `argv` w celu znalezienia odpowiedniego argumentu i jego ewentualnego parametru.

## See Also

Chociaż programowanie w języku C++ może czasem wydawać się trudne, to dzięki wielu dostępnym bibliotekom i funkcjom, obsługa takich elementów jak argumenty z linii poleceń może być prostsza i bardziej intuicyjna. Poniżej znajdziesz kilka przydatnych linków:

- [Dokumentacja języka C++](http://www.cplusplus.com/doc/)
- [Funkcje konwersji stringów](https://en.cppreference.com/w/cpp/string/basic_string/stol)
- [Obsługa opcji w języku C++](https://www.gnu.org/software/libc/manual/html_node/Basic-Getopt.html)