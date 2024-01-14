---
title:                "C#: Sprawdzanie istnienia katalogu"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą, który pracuje z językiem C#, na pewno musisz często sprawdzać, czy istnieje dany katalog. Jest to niezbędne w wielu projektach oraz może pomóc w uniknięciu błędów podczas uruchamiania aplikacji. W tym artykule dowiesz się, dlaczego warto sprawdzać istnienie katalogu oraz jak to zrobić w języku C#.

## Jak to zrobić

Sprawdzenie, czy dany katalog istnieje, jest bardzo prostym zadaniem w języku C#. Wystarczy użyć wbudowanej klasy "Directory" oraz jej metody "Exists". Poniższy kod przedstawia prosty przykład sprawdzenia, czy katalog "Użytkownicy" istnieje na dysku C:

```C#
if (Directory.Exists(@"C:\Użytkownicy"))
{
    Console.WriteLine("Katalog istnieje.");
}
else
{
    Console.WriteLine("Katalog nie istnieje.");
}
```

Wynik działania tego kodu będzie zależał od tego, czy w danym miejscu na dysku istnieje katalog o nazwie "Użytkownicy". Jeśli tak, w konsoli pojawi się informacja o jego istnieniu, w przeciwnym przypadku zostanie wyświetlony komunikat o jego braku.

## Deep Dive

Sprawdzanie istnienia katalogu może być również bardziej zaawansowane, jeśli potrzebujemy dostać się do jego podkatalogów lub plików. W tym celu możemy użyć metody "GetDirectories" dla klasy "Directory". Warto również pamiętać, że jeśli chcemy sprawdzić istnienie pliku, możemy użyć metody "File.Exists" zamiast "Directory.Exists".

## Zobacz również

- [Dokumentacja Microsoft o sprawdzaniu istnienia katalogu w języku C#](https://docs.microsoft.com/pl-pl/dotnet/standard/io/how-to-check-if-a-directory-exists) 
- [Przydatne informacje o innych metodach klasy "Directory"](https://www.tutorialspoint.com/csharp/csharp_directory_class.htm)
- [Przykładowe zastosowanie sprawdzania istnienia katalogu w aplikacji konsolowej](https://www.c-sharpcorner.com/blogs/how-to-check-if-a-directory-exits-in-c-sharp-programming1)