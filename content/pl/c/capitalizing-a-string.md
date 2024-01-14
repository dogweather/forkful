---
title:                "C: Zmiana pierwszej litery na wielką w ciągu znaków."
simple_title:         "Zmiana pierwszej litery na wielką w ciągu znaków."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Witajcie programiści! Jeśli jesteście zainteresowani poznaniem sposobu na wielką literę w stringu w języku C, to dobrym miejscem jest ten blog post. Kapitalizacja stringów jest niezwykle przydatna w wielu sytuacjach, na przykład w tworzeniu użytkowników do systemu czy tworzeniu nazw plików. Jest to również ważna umiejętność do posiadania przy pisaniu kodów klienckich dla systemów operacyjnych lub aplikacji internetowych.

## Jak To Zrobić

Zanim przejdziemy do kodów, powinniśmy wiedzieć, że istnieją dwa sposoby na wielką literę w stringu w języku C. Pierwszym sposobem jest użycie funkcji `toupper()` z biblioteki `ctype.h`, która zmienia wszystkie litery w stringu na ich odpowiednik w wielkich literach. Drugim sposobem jest ręczne zmienianie każdej litery na wielką literę, wykorzystując wiedzę o kodach ASCII. Oto przykładowy kod dla obu metod:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    // przykładowy string do kapitalizacji
    char str[] = "hej, to jest przykładowy string";
    
    // użycie funkcji toupper()
    int i = 0;
    while(str[i]) {
        str[i] = toupper(str[i]);
        i++;
    }
    printf("Wielki liter: %s\n", str);
    
    // ręczna kapitalizacja
    i = 0;
    while(str[i]) {
        // sprawdzenie czy litera jest mała, czyli w zakresie ASCII od 97 do 122
        if(str[i] >= 'a' && str[i] <= 'z') {
            // odejmowanie 32 od kodu ASCII dla zmiany na wielką literę
            str[i] = str[i] - 32;
        }
        i++;
    }
    printf("Wielki liter: %s\n", str);
    
    return 0;
}
```
**Output:**

```
Wielki liter: HEJ, TO JEST PRZYKLADOWY STRING
Wielki liter: HEJ, TO JEST PRZYKLADOWY STRING
```
Jak widać, oba sposoby dają ten sam wynik. Możesz wybrać którąkolwiek z nich, w zależności od Twoich preferencji.


## Deep Dive

Jeśli chcesz dowiedzieć się więcej o kapitalizacji stringów w języku C, istnieje kilka ważnych rzeczy, które należy wziąć pod uwagę. Po pierwsze, pamiętaj, że w języku C nie ma typu danych "string", więc musisz używać tablic znaków do przechowywania danych tekstowych. Aby uniknąć błędów, upewnij się, że rozmiar tablicy jest większy niż liczba znaków w stringu. Drugim ważnym aspektem jest fakt, że znaki specjalne, takie jak `?`, `&`, `%`, mogą wpłynąć na wynik kapitalizacji i należy z nimi uważać. Wreszcie, pamiętaj, aby zamknąć string znakiem null (`'\0'`) na końcu, aby oznaczyć jego koniec.

## Zobacz też

Jeśli chcesz pogłębić swoją wiedzę na temat języka C, oto kilka przydatnych linków:

- [Podstawy języka C](https://www.tutorialspoint.com/cprogramming/index.htm)
- [Kurs języka C na Codecademy](https://www.codecademy.com/learn/learn-c)
- [Oficjalna dokumentacja języka C](https://en.cppreference.com/w/c)

Dziękuję za przeczytanie mojego blog posta. Mam nadzieję, że był on dla Ciebie pomocny. Do zobaczenia w następnym wpis