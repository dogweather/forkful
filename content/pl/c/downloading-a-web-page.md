---
title:                "Pobieranie strony internetowej"
html_title:           "C: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobieranie strony internetowej to proces pobierania kodu źródłowego witryny z internetu. Programiści mogą to robić z różnych powodów, na przykład do analizy kodu lub pobierania danych z witryny.

## Jak to zrobić?
Poniżej przedstawiam kod w języku C, który można użyć do pobrania strony internetowej i wyświetlenia jej kodu źródłowego w konsoli:

```C
#include<stdio.h>
#include<stdlib.h>

int main(){
    // deklaracja i inicjalizacja zmiennej dla adresu URL
    char url[] = "https://www.example.com";
    // otwarcie strumienia dla adresu URL
    FILE *fp = fopen(url, "r");
    // ustawienie maksymalnego rozmiaru dla odczytywanych danych
    char buffer[100];
    // odczytywanie danych ze strumienia i wyświetlenie ich w konsoli
    while(fgets(buffer, 100, fp) != NULL){
        printf("%s", buffer);
    }
    // zamknięcie strumienia
    fclose(fp);
    return 0;
}
```
Wynikiem działania tego kodu będzie wyświetlenie kodu źródłowego strony internetowej w konsoli.

## Głębszy zanurzenie
Pobieranie stron internetowych jest powszechnie używanym zadaniem w programowaniu. W przeszłości, programiści musieli ręcznie pobierać kod źródłowy każdej witryny, co zajmowało im dużo czasu. Dzięki narzędziom, takim jak język C i biblioteka standardowa, można to robić w bardziej zautomatyzowany sposób.

Alternatywą dla pobierania stron internetowych jest wykorzystanie dedykowanych narzędzi do tego zadania, np. narzędzi w językach Python lub Ruby.

Implementacja pobierania stron internetowych jest zależna od konkretnego języka programowania. W języku C, jak w przykładzie powyżej, wykorzystuje się funkcję `fopen()` do otwarcia strumienia dla adresu URL, a następnie czyta się dane z tego strumienia i wyświetla w konsoli.

## Zobacz także
- [Dokumentacja biblioteki standardowej języka C](https://en.cppreference.com/w/c)
- [Narzędzia do automatyzacji pobierania stron internetowych w języku Python](https://realpython.com/python-web-scraping-libraries/)
- [Narzędzia do automatyzacji pobierania stron internetowych w języku Ruby](https://www.rubyguides.com/2019/10/ruby-web-scraping/)

Dzięki wykorzystaniu języka C, programiści są w stanie szybko i łatwo pobierać strony internetowe i wykorzystywać je do różnych celów. Warto zaznajomić się z tą funkcjonalnością, ponieważ może przydać się w wielu projektach.