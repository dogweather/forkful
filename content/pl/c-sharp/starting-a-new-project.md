---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Bash: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Rozpoczęcie nowego projektu w programowaniu oznacza tworzenie od podstaw nowej aplikacji lub oprogramowania. Programiści to robią, aby rozwiązać unikalne problemy lub stworzyć nowe funkcjonalności, które jeszcze nie istnieją.

## Jak to zrobić:

Uruchomienie nowego projektu w C# jest proste i można to zrobić za pomocą poniższego kodu:

```C#
using System;

namespace NowyProjekt
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Cześć, Świecie!");
        }
    }
}
```

Po uruchomieniu tego kodu, utworzy on prosty program wypisujący "Cześć, Świecie!".

## Deep Dive 

Historia tworzenia nowych projektów jest długa i złożona - zaczęło się to dawno temu, kiedy programowanie było jeszcze w powijakach. 

Alternatywą dla startu nowego projektu od zera jest sklonowanie istniejącego projektu i dopasowanie go do swoich potrzeb. To jest szybszy sposób, ale może prowadzić do problemów, jeśli źródłowy projekt nie jest dobrze zrozumiany.

Głębsze informacje o implementacji dotyczą takich aspektów jak struktura projektu, wybór odpowiedniego IDE, i wybór bibliotek, które będą w projekcie wykorzystane. Jak zawsze, decyzje wyboru zależą od specyfiki projektu.

## Zobacz Również 

Jeśli chcesz dowiedzieć się więcej, oto kilka pomocnych linków:

- Dokumentacja Microsoft C#: https://docs.microsoft.com/pl-pl/dotnet/csharp/
- Kreatywna online nauka programowania C#: https://www.codecademy.com/learn/learn-c-sharp
- Przewodnik programowania na platformie .NET: https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/