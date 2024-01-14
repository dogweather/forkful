---
title:                "C#: Usuwanie znaków pasujących do wzorca"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Dlaczego usuwanie znaków pasujących do wzoru jest przydatne w programowaniu

Usuwanie znaków pasujących do określonego wzoru jest przydatne wtedy, gdy chcemy szybko i efektywnie przetworzyć duże ilości tekstu, usuwając zbędne znaki. Może to również pomóc w poprawie wydajności naszego kodu poprzez zoptymalizowanie ilości przetwarzanych danych.

## Jak to zrobić w C#

Aby usunąć znaki pasujące do wzoru w C#, możemy skorzystać z klasy `Regex`. Najpierw musimy utworzyć obiekt `Regex`, podając jako parametr nasz wzór. Następnie wywołujemy metodę `Replace`, podając jako parametry tekst, w którym chcemy usunąć znaki, oraz pusty łańcuch znaków, co spowoduje usunięcie pasujących znaków. Oto przykładowy kod:

```C#
string tekst = "To jest przykładowy tekst, który zawiera znaki np. # i ?";
Regex wzor = new Regex("[#\\?]");
string przetworzony = wzor.Replace(tekst, "");
Console.WriteLine(przetworzony); // "To jest przykładowy tekst, który zawiera znaki np.  i "
```

W powyższym przykładzie użyliśmy wzoru `[#\\?]`, który pozwala usunąć wszystkie znaki # i ?. Aby dowiedzieć się więcej o różnych wzorach i ich składni, warto zapoznać się z dokumentacją na temat wyrażeń regularnych w C#.

## Wnikliwe spojrzenie

Usuwanie znaków pasujących do wzoru jest możliwe dzięki wykorzystaniu wyrażeń regularnych (ang. regular expressions), czyli tak zwanych „wzorców” lub „wyrażeń względnych”. Wyrażenia regularne są potężnym narzędziem w przetwarzaniu i manipulacji tekstem, pozwalającym na precyzyjne dopasowywanie i transformowanie znaków.

W C# wyrażenia regularne reprezentowane są przez klasę `Regex`, która udostępnia wiele przydatnych metod, takich jak `Replace` czy `Match`, oraz kilka sposobów tworzenia obiektów regex, w zależności od potrzeb. Klasa ta korzysta z wyrażeń regularnych opartych na silniku PCRE (Perl Compatible Regular Expressions), co pozwala na wygodną i wydajną pracę z zapisem wzorców.

Ważne jest również pamiętanie o wrażliwości na wielkość liter podczas korzystania z wyrażeń regularnych w C#. Domyślnie są one nieczułe na wielkość liter, ale możemy to zmienić, ustawiając odpowiednią opcję podczas inicjalizacji obiektu `Regex`.

## Zobacz także

- [Dokumentacja C# na temat wyrażeń regularnych](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Seria artykułów o wyrażeniach regularnych w C#](https://www.codeproject.com/Articles/9099/The-30-Minute-Regex-Tutorial)
- [Narzędzie do testowania wyrażeń regularnych w C#](https://regexr.com/)