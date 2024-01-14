---
title:                "C#: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego warto pisać do standardowego błędu?

Nierzadko w naszych programach pojawiają się błędy. Każdy programista wie, że ich poprawne zrozumienie i łatwe zlokalizowanie jest kluczowe w celu szybkiego i skutecznego rozwiązywania problemów. I właśnie dlatego warto wiedzieć, jak pisać do standardowego błędu w języku C#.

## Jak to zrobić?

Aby napisać do standardowego błędu w C#, należy użyć metody `Console.Error.WriteLine()`. Poniżej przedstawiam przykłady, jak można wykorzystać tę metodę do wypisywania błędów na konsoli:

```C#
Console.Error.WriteLine("Podany plik nie istnieje!");
```

W powyższym przykładzie wprowadzamy wiadomość błędu do metody `Console.Error.WriteLine()`, która następnie zostaje wypisana na standardowym błędzie. Aby umożliwić wyświetlanie liczby linii, w których występuje błąd, można wykorzystać obiekt `StackTrace`:

```C#
try
{
    // kod, który może generować błąd
}
catch (Exception ex)
{
    Console.Error.WriteLine("Wystąpił błąd: " + ex.Message);
    Console.Error.WriteLine("Liczba linii z błędem: " + ex.StackTrace);
}
```

W powyższym przykładzie po wykryciu błędu w bloku `try`, zostaje on przechwycony i wypisany na standardowym błędzie, razem z informacją o liczbie linii, w których wystąpił błąd.

## Głębsze zanurzenie

Warto wiedzieć, że w języku C# można również przekierować standardowy błąd do pliku tekstowego. Należy wtedy wykorzystać metodę `SetError()` klasy `Console`:

```C#
string path = "ErrorLog.txt";
FileStream fs = new FileStream(path, FileMode.Create);
Console.SetError(fs);
```

Powyższy przykład wykorzystuje obiekt `FileStream` do utworzenia pliku `ErrorLog.txt`, do którego następnie przekazujemy standardowy błąd.

## Zobacz też

- Dokumentacja C# o metodzie `Console.Error.WriteLine()`: https://docs.microsoft.com/pl-pl/dotnet/api/system.console.error.writeline
- Informacje o klasie `Console`: https://www.tutorialsteacher.com/csharp/csharp-console-application
- Przykłady wykorzystania standardowego błędu w C#: https://www.c-sharpcorner.com/UploadFile/532fc7/handle-exception-using-custom-errors/