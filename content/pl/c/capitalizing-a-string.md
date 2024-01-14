---
title:    "C: Zapisywanie napisu dużymi literami"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

W tym wpisie postaram się przybliżyć Wam jedną z podstawowych operacji w języku programowania C - zmianę dużych liter na małe (albo odwrotnie). Może to wydawać się proste i niepotrzebne - ale w rzeczywistości jest to bardzo ważne narzędzie w wielu programach. Czy zawsze musimy samodzielnie napisać tę funkcję? Oczywiście, że nie! Dzięki odpowiednim narzędziom można zaoszczędzić sobie czas, a co najważniejsze - zapewnić bezbłędność działania programu.

## Jak to zrobić?

Zacznijmy od klasycznego przykładu - mamy napisany pewien program, który wymaga od użytkownika podania imienia i nazwiska. Chcielibyśmy jednak, aby program przyjmował imiona i nazwiska tylko w formie zaczynającej się z wielkiej litery. W takiej sytuacji przydatna będzie funkcja "toupper" (nie mylić z funkcją "toupper" z biblioteki ctype.h, która zmienia znaki na wielkie litery). Przyjrzyjmy się poniższemu kodowi:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char name[20];
    char surname[30];
    printf("Podaj swoje imię: ");
    scanf("%s", name);
    printf("Podaj swoje nazwisko: ");
    scanf("%s", surname);
    name[0] = toupper(name[0]);
    surname[0] = toupper(surname[0]);
    printf("Twoje imię i nazwisko to: %s %s\n", name, surname);
	return 0;
}
```

Zwróćcie uwagę na linie 8 i 9 - to właśnie tam wykorzystujemy funkcję "toupper" do zmiany pierwszej litery na dużą. Dzięki temu, niezależnie od tego jak użytkownik wpisze swoje dane, program automatycznie zmieni pierwszą literę na dużą. Przykładowy output dla wpisanych imion "jan" i "kowalski" będzie wyglądał tak:

```
Podaj swoje imię: jan
Podaj swoje nazwisko: kowalski
Twoje imię i nazwisko to: Jan Kowalski
```

W ten sposób nie musimy martwić się o poprawność wprowadzonych danych.

## Deep Dive

Chciałbym jeszcze nieco dogłębniej przyjrzeć się funkcji "toupper". Jak już wcześniej wspomniałem, nie jest to jedyna funkcja tego typu. Biblioteka ctype.h oferuje również funkcję "tolower" - która zamienia znaki na małe litery. Dodatkowo, ta sama biblioteka udostępnia jeszcze kilka innych przydatnych funkcji. Są to między innymi "islower", "isupper" czy "isalpha". Są to funkcje, które pozwalają sprawdzić czy dany znak jest małą/dużą literą lub czy jest literą w ogóle. Dzięki temu, możemy sprawdzić czy użytkownik wprowadził tylko litery, a nie na przykład cyfry lub znaki specjalne.

Przykładowy kod wykorzystujący funkcje "tolower" oraz "islower" oraz "isalpha" może wyglądać tak:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char sentence[50];
    printf("Wpisz swoje zdanie: ");
    fgets(sentence, sizeof(sentence), stdin);
    for (int i = 0; sentence[i] != '\0'; i++) {
        if (isalpha(sentence[i])) {
            if (islower(sentence[i])) {
                sentence[i] = toupper(sentence[i]);
            } else {
                sentence[i] = tolower(sentence[i]);
            }
        }
    }
    printf("Odwrócone zdanie to: %s", sentence);
	return 0;
}
```
Zwróćcie uwagę na funkcję "tolower", której wynik jest przypisywany do zmienne