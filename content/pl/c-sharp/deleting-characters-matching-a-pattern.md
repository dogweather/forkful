---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Usuwanie Znaków Pasujących do Wzorca w C# 

## Co i dlaczego?
Usuwanie znaków pasujących do wzorca polega na wyszukaniu i usunięciu ciągu znaków zgodnych z określonym wzorcem z większego ciągu znaków. Programiści robią to, aby manipulować danymi, czyszcząc tekst lub modyfikując go dla konkretnych celów.

## Jak to zrobić:
Zapoznaj się z poniższym przykładem kodu, który ilustruje, jak usunąć znaki pasujące do wzorca 'abc' w C#:

```C#
string input = "To jestabc testabc.";
string pattern = "abc";
string output = input.Replace(pattern, "");
Console.WriteLine(output);
// Wyjście: "To jest test."
```
Jak widać, użyliśmy metody `Replace` w celu usunięcia wzorca z ciągu wejściowego.

## Głębsze spojrzenie
1. Kontekst historyczny: Usuwanie wzorców znaków jest często używane przez programistów od początków języków programowania. W pierwszych wersjach języka C# mnóstwo takich operacji wymagało korzystania z różnych technik i bibliotek, a prostota metody `Replace` wprowadzona w nowszych wersjach znacznie ułatwiła ten proces.

2. Alternatywy: Można również korzystać z wyrażeń regularnych (RegEx) do bardziej skomplikowanych wzorców znaków. Na przykład:

```C#
string input = "Test 123, test 456";
string output = Regex.Replace(input, @"\d", "");
Console.WriteLine(output);
// Wyjście: "Test , test "
```
W powyższym przykładzie, za pomocą RegEx, usunęliśmy wszystkie cyfry z ciągu wejściowego.

3. Szczegóły implementacji: W C#, metoda `Replace` działa poprzez przeszukiwanie ciągu znaków od lewej do prawej i zastępowanie każdego wystąpienia wzorca pustym ciągiem znaków – co skutkuje usunięciem tego wzorca.

## Zobacz także
Jak korzystać z wyrażeń regularnych w C#:  
https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expressions

Dokumentacja metody `Replace`:  
https://docs.microsoft.com/pl-pl/dotnet/api/system.string.replace?view=net-6.0

Te źródła dostarczą ci więcej informacji na temat usuwania znaków pasujących do wzorca i innych technik manipulacji ciągami w C#.