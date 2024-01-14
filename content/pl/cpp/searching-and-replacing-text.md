---
title:    "C++: Wyszukiwanie i zastępowanie tekstu"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego
Czy kiedykolwiek zdarzyło Ci się potrzebować zmienić fragment tekstu w swoim kodzie C++? Może chciałeś zamienić wszystkie wystąpienia pewnego słowa na inne, lub po prostu poprawić błąd w nazwie zmiennej. W takich przypadkach, narzędzie do szukania i zamiany tekstu może okazać się niezbędnym elementem Twojego arsenału programisty.

## Jak to zrobić
W C++, szukanie i zamiana tekstu odbywa się za pomocą funkcji `find()` i `replace()`, dostępnych w standardowej bibliotece `string`. Poniższy przykład pokazuje jak użyć tych funkcji do zamiany słowa "wyraz" na "słowo" w zdaniu "Ten wyraz jest błędny.".

```C++
#include <iostream>
#include <string>

using namespace std;

int main(){
    string sentence = "Ten wyraz jest błędny.";
    size_t found = sentence.find("wyraz");
    
    if(found != string::npos){ //sprawdza czy znaleziono szukany wyraz
        sentence.replace(found, 5, "słowo"); //zmienia wyraz "wyraz" na "słowo"
        cout << sentence << endl; //wypisuje "Ten słowo jest błędny."
    }
    return 0;
}
```

Mamy tutaj zmienną `found`, która przechowuje indeks pierwszego znalezionego wystąpienia szukanego wyrazu. Jeśli jego wartość nie jest równa `string::npos`, co oznacza brak wyniku, możemy użyć funkcji `replace()` aby zamienić znaleziony wyraz na nowy. W kolejnym kroku wypisujemy zmienioną wersję zdania.

## Głębsze zagłębienie
Dodatkowo, funkcje `find()` i `replace()` posiadają różne argumenty i możliwości konfiguracji, które mogą być przydatne w zależności od konkretnej sytuacji. Na przykład, możemy określić od którego indeksu poszukiwanie ma się rozpocząć, lub jak dużo wystąpień ma zostać zamienione. Dodatkowo, istnieją również inne funkcje do szukania i zamiany tekstu, takie jak `regex_replace()`, które posiadają jeszcze większe możliwości.

## Zobacz także
- Dokumentacja funkcji `find()` i `replace()` w języku C++: http://www.cplusplus.com/reference/string/string/find/
- Przykładowe zastosowania funkcji `regex_replace()`: https://www.geeksforgeeks.org/regex-regular-expression-in-c/
- Praktyczne porady dotyczące szukania i zamiany tekstu w C++: https://www.codingame.com/playgrounds/2205/7-cool-pattern-matching-features-of-c17