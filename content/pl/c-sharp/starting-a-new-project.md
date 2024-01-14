---
title:    "C#: Rozpoczęcie nowego projektu"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego

Rozpoczęcie nowego projektu może być wyzwaniem, ale może również być źródłem wielkiej satysfakcji. Możesz zacząć tworzyć coś zupełnie nowego, wykorzystując swoje umiejętności programistyczne i kreatywność.

## Jak to zrobić

Aby rozpocząć nowy projekt w C#, musisz mieć zainstalowane oprogramowanie Visual Studio. Następnie możesz utworzyć nowy projekt, wybierając odpowiednią opcję z menu "File". Możesz wybrać różne szablony projektów, w zależności od swoich potrzeb.

W poniższym przykładzie stworzymy prosty kalkulator, który będzie dodawał dwie liczby wprowadzone przez użytkownika.

```C#
using System;

namespace Calculator
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Wprowadź pierwszą liczbę:");
            int num1 = Convert.ToInt32(Console.ReadLine());

            Console.WriteLine("Wprowadź drugą liczbę:");
            int num2 = Convert.ToInt32(Console.ReadLine());

            int result = num1 + num2;

            Console.WriteLine("Wynik dodawania: " + result);
            Console.ReadKey();
        }
    }
}
```

Po uruchomieniu programu, zostaniemy poproszeni o podanie dwóch liczb, a następnie zostanie wyświetlony wynik dodawania.

## Głębsze zanurzenie

Zanim zaczniesz tworzyć nowy projekt, warto spędzić nieco czasu na odpowiednim zaplanowaniu. To ważne, abyś miał jasne cele, wiedział jakie funkcjonalności chcesz zaimplementować i jakie struktury danych wykorzystać.

Kolejnym ważnym krokiem jest tworzenie czystego i czytelnego kodu. Dzięki temu łatwiej będzie Ci się orientować w swoim projekcie, a także będzie łatwiej wprowadzać późniejsze zmiany.

## Zobacz także

- [Podstawowe pojęcia w programowaniu w języku C#](https://4programmers.net/Artykuly/Podstawy_programowania_w_C_Sharp)
- [Jak wykonać pierwszy krok w programowaniu w języku C#](https://devstyle.pl/2016/03/29/1-krok-programowanie-w-jezyku-c/)
- [Tworzenie i uruchamianie nowego projektu w Visual Studio](https://docs.microsoft.com/pl-pl/visualstudio/get-started/tutorial-console?view=vs-2019)