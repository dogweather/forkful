---
title:                "Usuwanie znaków pasujących do wzorca"
date:                  2024-01-20T17:41:46.636797-07:00
model:                 gpt-4-1106-preview
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Usuwanie znaków pasujących do wzorca polega na wybraniu i wyeliminowaniu określonych sekwencji znaków z tekstu. Programiści robią to, żeby oczyścić dane, usunąć niechciane fragmenty, czy przygotować tekst do dalszego przetwarzania.

## How to: (Jak to zrobić:)
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string input = "Hello, World! 1234";
        string pattern = @"\d"; // \d to wzorzec dla cyfr
        
        string output = Regex.Replace(input, pattern, "");
        Console.WriteLine(output); // Wyświetli: Hello, World! 
    }
}
```
Sample Output:
```
Hello, World!
```

## Deep Dive (Wgłębienie się)
Usuwanie znaków stosowane jest już od czasów pierwszych komputerów. Jakiekolwiek przetwarzanie tekstu na wczesnych maszynach wymagało manipulacji ciągów znaków. W C# używamy klas z przestrzeni nazw `System.Text.RegularExpressions`, jak `Regex`, do pracy z wyrażeniami regularnymi. Są one potężne gdyż pasują do wzorców, a nie tylko do konkretnych znaków.

Alternatywnie, można użyć metod takich jak `String.Replace()` do usuwania określonych znaków lub `String.IndexOf()` w połączeniu z `String.Remove()` do bardziej skomplikowanych operacji.

Regex jest szybki i elastyczny, ale może być trudniejszy w zrozumieniu i debugowaniu. `Replace()` i inne metody `String` są proste, ale mogą wymagać więcej kodu i być mniej wydajne przy skomplikowanych wzorcach.

## See Also (Zobacz też)
- Dokumentacja Microsoft o klasie Regex: [https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex)
- Tutorial dotyczący wyrażeń regularnych: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
- Strona do testowania wyrażeń regularnych online: [https://regexr.com/](https://regexr.com/)
