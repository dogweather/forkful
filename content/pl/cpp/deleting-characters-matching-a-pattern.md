---
title:    "C++: Usuwanie znaków pasujących do wzorca"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest nieodłączną częścią naszego życia. Jest to umiejętność, która może otworzyć wiele drzwi i zapewnić wiele możliwości dla naszej kariery zawodowej. W tym wpisie dowiesz się, jak usuwać znaki odpowiadające wzorcowi w języku C++, co jest ważną umiejętnością dla każdego programisty.

## Jak To Zrobić

Aby usunąć znaki odpowiadające określonemu wzorcowi, możemy skorzystać z funkcji "erase" w języku C++. Funkcja ta jest dostępna dla obiektów typu "string" i pozwala nam na usunięcie znaków z zadanego zakresu, który możemy określić przy użyciu iteratorów. Poniżej przedstawiony jest prosty przykład kodu, który usuwa z ciągu znaki odpowiadające wzorcowi "abc".

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
   string str = "defabcghi";
   
   // Usuwanie znaków odpowiadających wzorcowi "abc"
   str.erase(str.begin()+3, str.end());
   
   // Wyświetlenie wyniku
   cout << str; // wyświetli "def"
   
   return 0;
}
```

W powyższym przykładzie mamy zmienną "str", która przechowuje ciąg znaków "defabcghi". Następnie używamy funkcji "erase" i przekazujemy jej jako argumenty iteratory na początek i koniec zakresu, który chcemy usunąć. W tym przypadku zaczynamy od 4. znaku (począwszy od 0) i usuwamy wszystkie znaki do końca ciągu. Wynikiem jest ciąg znaków "def".

## Deep Dive

Funkcja "erase" jest bardzo użyteczna i pozwala na wiele różnych możliwości manipulacji ciągami znaków. Możemy również określić tylko jeden iterator, który wskazuje na pozycję, od której chcemy zacząć usuwać. W takim przypadku wszystkie znaki po tym iteratorze zostaną usunięte. Możemy także określić tylko liczbę znaków do usunięcia, a funkcja "erase" sama znajdzie iterator na odpowiedniej pozycji i usunie dane znaki.

Ponadto, funkcja "erase" ma również wersję z 3 argumentami, w których możemy podać iterator na początek zakresu, iterator na koniec zakresu oraz iterator na miejsce, od którego chcemy wstawić nowe znaki. Dzięki temu możemy skrócić ciąg znaków, ale także dodać do niego nowe znaki na wybranej pozycji.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o funkcji "erase" i manipulacji ciągami znaków w języku C++, polecamy zapoznać się z poniższymi źródłami:

- [Dokumentacja funkcji "erase" na stronie cplusplus.com](https://www.cplusplus.com/reference/string/string/erase/)
- [Tutorial z wyjaśnieniem funkcji "erase" na stronie learncpp.com](https://www.learncpp.com/cpp-tutorial/learn-cpp-without-introducing-stl/)

Dzięki tej umiejętności będziesz w stanie szybko i sprawnie manipulować ciągami znaków w swoich programach. Życzymy powodzenia w dalszej nauce programowania!