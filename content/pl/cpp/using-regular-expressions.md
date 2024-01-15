---
title:                "Używanie wyrażeń regularnych"
html_title:           "C++: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regular expressions to bardzo przydatne narzędzie w programowaniu C++. Pozwalają one na wyszukiwanie i manipulowanie tekstem w sposobu niezwykle precyzyjnym i skutecznym. Ich użycie jest niezbędne w wielu przypadkach, np. w walidacji danych wejściowych, przetwarzaniu plików tekstowych, czy tworzeniu wyrażeń logicznych.

## Jak to zrobić

Aby zacząć używać regular expressions w C++, należy najpierw dołączyć bibliotekę <regex> do swojego kodu. Następnie, używając konstrukcji ```regex pattern("wzorzec");``` możemy utworzyć nasze pierwsze wyrażenie regularne. Aby wyszukać dopasowanie w danym tekście, używamy metody ```regex_search()```. Przykładowy kod wyglądać może następująco:

```
#include <iostream>
#include <regex>
using namespace std;

int main() {
	// tworzenie wyrażenia regularnego
	regex pattern("ala+[0-9]");

	// tekst, w którym szukamy dopasowań
	string tekst = "ala123456 trzyma kotka na końcu balustrady.";

	// sprawdzenie, czy występuje dopasowanie
	if (regex_search(tekst, pattern)){
		cout << "Tekst zawiera dopasowanie.";
	}
	else{
		cout << "Brak dopasowania.";
	}

	return 0;
}
```

Po uruchomieniu, powyższy kod wyświetli informację o występowaniu dopasowania w tekście.

## Głębszy zanurzenie

Regular expressions to bardzo potężne narzędzie, ale wymagają one trochę wprawy w użyciu. Obecnie większość edytorów i IDE posiada funkcje automatycznego generowania wyrażeń regularnych, co znacznie ułatwia pracę. Ponadto, warto pamiętać o specjalnych znakach i składni używanych w regular expressions, takich jak np. "*", "?", czy "[]". Dzięki nim możemy precyzyjnie określić poszukiwane ciągi znaków. Regular expressions są również często wykorzystywane w wielu innych językach programowania, więc nauczenie się ich jest przydatne nie tylko w kontekście C++.

## Zobacz także

- Dokumentacja biblioteki <regex> dla C++: https://en.cppreference.com/w/cpp/regex
- Przykładowe wyrażenia regularne: https://regex101.com/
- Wprowadzenie do regular expressions: https://www.regular-expressions.info/