---
title:                "Używanie wyrażeń regularnych."
html_title:           "C++: Używanie wyrażeń regularnych."
simple_title:         "Używanie wyrażeń regularnych."
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Co i dlaczego?
Regularne wyrażenia to sposób na wyszukiwanie i manipulowanie tekstami. Programiści używają ich, aby skrócić i ułatwić działanie na tekście.

# Jak to zrobić:
Przykłady kodów i wyniki działania przedstawione zostaną w bloku kodowym ```C++ ... ```

## Przykład 1:
```
#include <iostream>
#include <regex>
using namespace std;

int main(){
  string text = "Przykladowy tekst do znalezienia";
  regex expression("tekst");
  bool result = regex_search(text, expression);
  cout << "Czy wyrazenie zostalo znalezione? " << result << endl;
  return 0;
}
```
(OUTPUT: Czy wyrazenie zostalo znalezione? 1)

## Przykład 2:
```
#include <iostream>
#include <regex>
using namespace std;

int main(){
  string text = "Jakis inny przykladowy tekst";
  regex expression("tekst");
  string result = regex_replace(text, expression, "polecenie");
  cout << result << endl;
  return 0;
}
```
(OUTPUT: Jakis inny przykladowy polecenie)

## Przykład 3:
```
#include <iostream>
#include <regex>
using namespace std;

int main(){
  string text = "123456789";
  regex expression("^(\\d{3,4})(\\d{2})(\\d{2})$");
  smatch matches;
  regex_match(text, matches, expression);
  cout << "Wynik:" << endl;
  for (size_t i = 0; i < matches.size(); ++i) {
  	cout << "[" << i << "]: " << matches[i] << endl;
  }
  return 0;
}
```
(OUTPUT: Wynik: [0]: 123456789 [1]: 1234 [2]: 56 [3]: 89)

# Głębsza analiza:
Pierwsze implementacje regularnych wyrażeń pojawiły się w Unixie w latach 70. Dzisiaj są powszechnie używane we wszystkich językach programowania. Alternatywami dla wyrażeń regularnych są funkcje napisane specjalnie dla manipulacji tekstem lub biblioteki dla konkretnych języków. W C++ wykorzystuje się bibliotekę <regex>, która wprowadziła nowe funkcje udostępniane przez klasę regular_expression.

# Zobacz też:
- [Regular expressions for beginners](https://www.regular-expressions.info/tutorial.html)
- [C++ Reference: regex](https://en.cppreference.com/w/cpp/regex)
- [Regex Tester](https://regex101.com/) (narzędzie online do testowania i weryfikacji wyrażeń regularnych)