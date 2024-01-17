---
title:                "Wydobywanie podciągów"
html_title:           "C#: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

<h1> O co chodzi i dlaczego? </h1> 

W programowaniu pojęcie "wyodrębniania podciągów" odnosi się do pobrania części tekstu lub ciągu znaków z większego ciągu. Programiści często używają takiej operacji, aby manipulować danymi lub uzyskać potrzebne im informacje w łatwy i szybki sposób. 

<h1> Jak to zrobić: </h1>

Aby wyodrębnić podciągi w C#, istnieje kilka sposobów. Możemy użyć metody `Substring()` lub zastosować `Split()` wraz z odpowiednim separatorem. Poniżej znajdują się przykładowe kody, które ilustrują te dwie techniki:

```C#
string imie = "Ania, Basia, Kasia, Ola";
string[] imiona = imie.Split(','); //dzielimy tekst na podciągi rozdzielone przecinkiem

foreach (string imie in imiona)
{
    Console.WriteLine(imie);
}
```

W rezultacie powinniśmy zobaczyć wydruk w konsoli z podanymi imionami w kolejności: Ania, Basia, Kasia, Ola. 

Inną metodą jest użycie metody `Substring()` do wyodrębnienia fragmentu wybranego przez nas tekstu:

```C#
string oryginalnyTekst = "Ten tekst jest bardzo długi.";
string fragment = oryginalnyTekst.Substring(4, 10); //wybieramy fragment zaczynając od indeksu 4 i o długości 10 znaków

Console.WriteLine(fragment);
```

Tym razem wynik powinien zawierać tylko wybrany fragment, czyli "tekst jest".

<h1> Rzuć okiem na: </h1>

Jeśli chcesz dowiedzieć się więcej o wyodrębnianiu podciągów w C#, zapoznaj się z dokumentacją Microsoft na temat metody `Substring()` i `Split()`. Możesz także przetestować różne kombinacje i zastosowania tych metod w celu lepszego zrozumienia działania. 

<h1> Głębszy zanurzenie: </h1>

Praktyka wyodrębniania podciągów jest stosowana od dawna w wielu językach programowania. W przypadku C#, ta operacja jest wykonywana na typie `string`, który jest niezmienny. Oznacza to, że każda operacja zmieniająca tekst, tak jak wyodrębnianie podciągów, tworzy nowy obiekt i alokuje dla niego nową pamięć. 

Jeśli chcemy wydajniej pracować z wyodrębnianiem podciągów, możemy skorzystać z klasy `StringBuilder`, która jest zoptymalizowana do wykonywania wielu operacji na tekście bez tworzenia nowych obiektów przy każdej zmianie. Jest to szczególnie przydatne przy pracach na większych ciągach znaków. 

<h1> Zobacz także: </h1>

 - Dokumentacja przejrzystości kodu C# Microsoft: https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/concepts/coding-style 
 - Przydatne porady dotyczące optymalizacji kodu w C#: https://stackify.com/optimize-c-sharp-code/
 - Wideo tutorial dotyczące praktycznego zastosowania wyodrębniania podciągów: https://www.youtube.com/watch?v=rb2skyIuz2A