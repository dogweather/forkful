---
title:                "C#: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w procesie programowania musimy dokonać modyfikacji tekstu, na przykład usunąć pewne znaki lub słowa. Istnieje wiele metod, aby to zrobić, ale dziś skupimy się na usuwaniu znaków odpowiadających określonemu wzorcowi. Będziemy wyjaśniać dlaczego czasami potrzebujemy wykonać taką operację i jak możemy to zrobić w prosty sposób w języku C#.

## Jak To Zrobić

Aby usunąć znaki odpowiadające wzorcowi, musimy wykorzystać metodę `Regex.Replace()` wraz z wyrażeniem regularnym reprezentującym nasz wzorzec. Wyjaśnimy to na przykładzie usuwania liczb z ciągu znaków:

```C#
string text = "To jest 123 przykładowe 45 zdanie 678";
string pattern = "[0-9]"; // wzorzec to dowolna cyfra od 0 do 9

var result = Regex.Replace(text, pattern, ""); // usuwanie znaków odpowiadających wzorcowi

Console.WriteLine(result); // wyświetli "To jest przykładowe zdanie"
```

W tym przykładzie, wyrażenie regularne `[0-9]` reprezentuje pojedynczą cyfrę od 0 do 9. Natomiast metoda `Regex.Replace()` zastępuje wszystkie znalezione dopasowania wzorca pustym ciągiem znaków, przez co są one usuwane. Możemy również wykorzystać bardziej złożone wyrażenia regularne, aby usuwać większe fragmenty tekstu, na przykład słowa zaczynające się na "a" lub kończące się na "b".

## Deep Dive

Jeśli chcielibyśmy dowiedzieć się więcej na temat wyrażeń regularnych i ich zastosowań, możemy sięgnąć po specjalne książki, kursy online lub artykuły na ten temat. Istnieje również wiele przydatnych narzędzi, które pomagają w tworzeniu i testowaniu wyrażeń regularnych, na przykład [regex101](https://regex101.com/).

Warto pamiętać, że operacje na wyrażeniach regularnych mogą być czasochłonne, dlatego dobrą praktyką jest mierzenie wydajności naszego kodu i dostosowywanie go optymalnie do naszych potrzeb.

## Zobacz także

- [Dokumentacja języka C# na temat wyrażeń regularnych](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expressions)
- [Książka "Wyrażenia regularne. Wprowadzenie" na stronie Helion](https://helion.pl/ksiazki/wyrazenia-regularne-wprowadzenie-katrina-owens-vivien-barrous,weicere.htm)