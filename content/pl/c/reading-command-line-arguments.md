---
title:                "C: Odczytywanie argumentów wiersza poleceń"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy zdarzyło Ci się kiedykolwiek próbować uruchomić program z linii poleceń i nie wiedzieć, jak przekazać mu odpowiednie argumenty? A może spotkałeś program, który przyjmuje opcje w wierszu poleceń, ale nie wiesz, jak ich użyć? W tym blogu dowiesz się, jak czytać argumenty wiersza poleceń w języku programowania C.

## Jak to zrobić

Aby czytać argumenty z linii poleceń w C, potrzebne są nam dwie rzeczy: funkcja `main()` i parametry `argc` oraz `argv`. Pierwszy parametr `argc` jest liczbą argumentów przekazanych do programu, natomiast drugi parametr `argv` jest tablicą przechowującą te argumenty. Sprawdźmy teraz przykład, w którym użyjemy tej funkcji:

```C
#include<stdio.h>
 
int main(int argc, char *argv[])
{
    printf("Liczba argumentów: %d\n", argc);

    for(int i = 0; i < argc; i++)
    {
        printf("Argument %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

W powyższym kodzie wykorzystujemy pętlę `for` do przeiterowania przez wszystkie dostarczone argumenty i wyświetlenia ich na ekranie. Oto przykładowy wynik:

```
> ./program arg1 arg2 arg3
Liczba argumentów: 4
Argument 0: ./program
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```

Jak widać, jako pierwszy argument, zawsze podawana jest nazwa wykonanego programu.

## Głębsze spojrzenie

W języku C można również przekazywać opcje wiersza poleceń za pomocą flag. Możemy to zrobić, dodając przed argumentem "-" lub "--". W tym przypadku, będziemy potrzebowali funkcji `getopt()` oraz struktury `option`. Przykładowy kod może wyglądać jak poniżej:

```C
#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>
#include<getopt.h>
 
int main(int argc, char *argv[])
{
    int opt;

    while((opt = getopt(argc, argv, "ab:")) != -1)
    {
        switch(opt)
        {
            case 'a':
                printf("Opcja -a została użyta\n");
                break;
            case 'b':
                printf("Opcja -b została użyta z argumentem %s\n", optarg);
                break;
            case '?':
                if(optopt == 'b')
                {
                    printf("Opcja -b wymaga argumentu\n");
                }
                else
                {
                    printf("Niepoprawna opcja -%c\n", optopt);
                }
                break;
        }
    }

    return 0;
}
```

W powyższym kodzie używamy funkcji `getopt()` do odczytywania opcji i argumentów. Możemy także określić, które opcje wymagają argumentów, a także obsłużyć nieistniejące opcje lub brak argumentów dla wymaganych opcji. Przykładowo, jeśli uruchomimy powyższy program z flagą `-b` bez podania argumentu, dostaniemy komunikat o błędzie.

## Zobacz także

- [Dokumentacja C: main() function](https://www.tutorialspoint.com/cprogramming/c_main.htm)
- [Tutorial o czytaniu argumentów z linii poleceń w C](https://www.learn-c.org/en/Command_Line_Arguments)
- [Przykłady flag i opcji wiersza poleceń w C](https://www.geeksforgeeks.org/getopt-function-in-c-to-parse-command-line-arguments/)