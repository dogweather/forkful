---
title:    "C#: Ekstrakcja podciągów"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Dlaczego warto poznać ekstrakcję podciągów w języku C#?

Znajomość sposobu na wydobycie podciągów jest niezwykle przydatna w programowaniu w języku C#. Umożliwia ona otrzymanie części tekstu z ciągu znaków, co pozwala na bardziej precyzyjną manipulację danymi i ułatwia pracę ze złożonymi zmiennymi tekstowymi. Jeżeli chcesz nauczyć się ekstrakcji podciągów w C#, ten wpis jest dla Ciebie!

## Jak to zrobić?

Kodowanie w języku C# jest niezwykle proste i intuicyjne. W celu wydobycia podciągu z ciągu znaków używamy metody ```Substring()```, która jest dostępna dla typów danych String oraz StringBuilder.

Poniżej znajdują się przykładowe kody w języku C#, które demonstrują wydobycie podciągów z ciągu tekstowego oraz wyświetlenie ich na konsoli:

```
// C# Program do wydobycia podciągu
using System; 
public class Main  
{ 
    public static void Main(string[] args)  
    {  
        string text = "Witaj, jestem programem napisanym w języku C#.";  
        
        // Wydobywanie podciągów z użyciem metody Substring()
        Console.WriteLine("Pierwszy podciąg: " + text.Substring(7, 14)); 
        Console.WriteLine("Drugi podciąg: " + text.Substring(28)); 
        
        // Wydobywanie podciągów z użyciem znaczników indeksów
        Console.WriteLine("Trzeci podciąg: " + text[18..27]); 
        Console.WriteLine("Czwarty podciąg: " + text[0..7] + text[^7..]);
        Console.ReadKey(); 
    } 
}
```

Powyższy kod wyświetli następujące wyniki:

```
Pierwszy podciąg: jestem programem
Drugi podciąg: w języku C#.
Trzeci podciąg: napisanym
Czwarty podciąg: Witaj, C#.
```

W przypadku użycia metody ```Substring()```, podajemy dwa parametry - początkowy indeks oraz długość wydobywanego podciągu. Aby wydobycie było jeszcze wygodniejsze, język C# wprowadził również znaczniki indeksów (ang. indexers). Pozwalają one na wybieranie podciągów na podstawie ich początkowego i końcowego indeksu, gdzie ```[..]``` oznacza początek ciągu znaków, a ```[^..]``` oznacza koniec. Podajesz wartość indeksu pomiędzy znakami ```..```.

## Ciekawostki na temat ekstrakcji podciągów w języku C#

- Aby wyciąć ostatni znak z ciągu znaków, możesz użyć wyrażenia ```text[..^1]```.
- Możesz wybierać podciągi, podstawiając za edne miejsce parametr ```0```, np. ```text[0..20]```.
- Jeśli podasz wartość indeksu większą niż długość ciągu znaków, zostanie wyświetlony błąd.

## Zobacz również
- [Oficjalna dokumentacja języka C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/)
- [Wprowadzenie do stringów w języku C#](https://www.tutorialsteacher.com/csharp/csharp-string)