---
title:    "C: Odczytywanie argumentów z wiersza poleceń"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

#Dlaczego

W dzisiejszych czasach, programiści często mają do czynienia z interakcją użytkowników z ich programami poprzez wiersz poleceń. W takich przypadkach, wiadomości przekazane przez użytkownika podczas uruchamiania programu mogą mieć duże znaczenie dla jego działania. Dlatego jest ważne, aby umieć prawidłowo odczytać argumenty wiersza poleceń poprzez kodowanie w języku C.

#Jak to zrobić

Aby odczytać argumenty wiersza poleceń w języku C, musimy użyć funkcji \texttt{argc} i \texttt{argv}. Pierwszy argument (\texttt{argc}) to liczba całkowita, która przechowuje ilość podanych argumentów wiersza poleceń, a drugi argument (\texttt{argv}) to tablica ciągów znaków, która zawiera te argumenty.

Przykładowy kod poniżej pokazuje, jak użyć tych funkcji w praktyce:

```c
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Liczba argumentów: %d\n", argc);

    for (int i = 0; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

Kiedy uruchomimy ten program z kilkoma argumentami, na przykład: ```./program argument1 argument2 argument3```, otrzymamy następujący wynik:

```
Liczba argumentów: 4
Argument 0: ./program
Argument 1: argument1
Argument 2: argument2
Argument 3: argument3
```

Mamy dostęp do każdego podanego argumentu poprzez indeksowanie tablicy \texttt{argv}.

#Głębszy zanurzanie

W wierszu poleceń możemy również używać opcjonalnych argumentów, które są poprzedzone znakiem ``-`` lub ``--``. W celu obsłużenia tych argumentów, musimy zastosować funkcję \texttt{getopt()}, która pozwala nam odczytać argumenty opcjonalne oraz ich wartości. Poniższy przykład prezentuje to w praktyce:

```c
#include <stdio.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
    int opt;

    while((opt = getopt(argc, argv, "abc:")) != -1) {
        switch(opt) {
            case 'a':
                printf("Argument opcjonalny '-a' podany\n");
                break;
            case 'b':
                printf("Argument opcjonalny '-b' podany\n");
                break;
            case 'c':
                printf("Argument opcjonalny '-c' podany z wartością: %s\n", optarg);
                break;
            default:
                printf("Nieznany argument opcjonalny\n");
        }
    }

    return 0;
}
```

W powyższym przykładzie określiliśmy, że nasz program akceptuje argumenty opcjonalne ``-a``, ``-b`` oraz ``-c``, który może mieć wartość. Więcej informacji na temat użycia funkcji ``getopt()`` znajduje się w jej dokumentacji.

#Zobacz także

- [Dokumentacja funkcji \texttt{argc} i \texttt{argv}](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html#Program-Arguments)
- [Dokumentacja funkcji \texttt{getopt()}](https://www.gnu.org/software/libc/manual/html_node/Getopt.html#Getopt)