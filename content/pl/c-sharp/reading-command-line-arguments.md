---
title:                "C#: Odczytywanie argumentów linii poleceń"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą C#, z pewnością spotkałeś się z pojęciem argumentów linii poleceń. Te małe, ale potężne elementy pozwalają na przekazywanie parametrów do aplikacji podczas jej uruchamiania. Ale dlaczego warto poznać więcej na ten temat? Czytaj dalej, aby dowiedzieć się dlaczego warto poznać i umiejętnie wykorzystywać argumenty linii poleceń w swoich projektach.

## Jak to zrobić

Aby zacząć pracę z argumentami linii poleceń w C#, możesz skorzystać z metody `Main` w klasie `Program`. Następnie, wykorzystując tablicę `args`, możesz odczytać i przetworzyć przekazane parametry. Przykładowy kod wygląda tak:

```C#
static void Main(string[] args)
{
    foreach(string arg in args)
    {
        Console.WriteLine(arg);
    }
}
```

Po uruchomieniu aplikacji z parametrami `argument1 argument2` otrzymasz następujący wynik:

```
argument1
argument2
```

W ten sposób możesz pobierać i wykorzystywać argumenty w swoich programach. Warto również pamiętać, że możesz przekazywać nie tylko wartości tekstowe, ale również liczby czy flagi, które mogą określać działanie aplikacji.

## Deep Dive

Jeśli chcesz poznać więcej o argumentach linii poleceń w C#, istnieje kilka ważnych zagadnień, które warto zrozumieć. Jednym z nich jest ich kolejność w tablicy `args`, która może mieć znaczenie, jeśli chcesz przetwarzać różne rodzaje argumentów. Kolejną ważną kwestią jest konwersja typów, ponieważ wszystkie elementy w tablicy są typu `string`.

Rozważmy przykład, w którym jako pierwszy argument przekazujemy liczbę, a jako drugi flagę. Wówczas nasz kod może wyglądać tak:

```C#
static void Main(string[] args)
{
    int number = Convert.ToInt32(args[0]);
    bool flag = Convert.ToBoolean(args[1]);
    
    Console.WriteLine(number * 2);
    Console.WriteLine(flag);
}
```

Po uruchomieniu z parametrami `5 true` otrzymamy wynik:

```
10
True
```

Dzięki temu, możesz dowolnie przetwarzać przekazane argumenty, zależnie od potrzeb twojego projektu.

## Zobacz również

- [Dokumentacja Microsoft dotycząca argumentów linii poleceń w C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/)
- [Przykład wykorzystania argumentów linii poleceń w C#](https://dotnettutorials.net/lesson/csharp-command-line-arguments/)
- [Poradnik poświęcony argumentom linii poleceń w C#](https://www.c-sharpcorner.com/article/working-with-command-line-arguments-in-c-sharp/)