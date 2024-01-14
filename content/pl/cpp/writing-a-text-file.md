---
title:    "C++: Tworzenie pliku tekstowego"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Dlaczego pisać plik tekstowy?

Pisanie plików tekstowych jest nieodłączną częścią programowania w C++. Są one wykorzystywane do przechowywania danych, tworzenia raportów lub komunikacji z innymi programami. Pisanie plików tekstowych jest niezbędnym umiejętnością, jeśli chcemy tworzyć zaawansowane programy.

# Jak to zrobić?

Pisanie plików tekstowych w C++ jest bardzo proste i wymaga tylko kilku linii kodu. Najpierw należy otworzyć plik za pomocą funkcji `fstream`, określając nazwę pliku i tryb dostępu. Następnie możemy użyć funkcji `<<` do zapisania danych do pliku, lub `>>` do odczytu danych z pliku. Na przykład:

```C++
// otwórz plik do zapisu
fstream plik("dane.txt", ios::out);
// zapisz dane do pliku
plik << "Witaj, świecie!" << endl;
// zamknij plik
plik.close();
```

Aby odczytywać dane z pliku, użyjemy podobnego kodu, zmieniając tylko tryb dostępu na `ios::in`. Oto przykładowy kod odczytujący dane z pliku:

```C++
// otwórz plik do odczytu
fstream plik("dane.txt", ios::in);
// zmienna do przechowywania odczytanych danych
string dane;
// odczytaj dane z pliku i zapisz do zmiennej
plik >> dane;
// zamknij plik
plik.close();
// wydrukuj odczytane dane
cout << dane << endl; // wyświetli "Witaj, świecie!"
```

# Głębsza analiza

W C++ istnieje kilka innych funkcji, które można wykorzystać przy pisaniu i odczytywaniu plików tekstowych. Jedną z nich jest funkcja `getline()`, która pozwala nam odczytać całą linię tekstu z pliku za jednym razem. Innymi przydatnymi funkcjami są `tellg()` i `seekg()`, które pozwalają nam ustalić aktualną pozycję w pliku i przesunąć się do innej pozycji. Warto również zapoznać się z pojęciem bufora pliku, aby lepiej zrozumieć, jak działają operacje wejścia/wyjścia na plikach.

# Zobacz też

- [Podstawy programowania w C++](https://www.codecademy.com/learn/learn-c-plus-plus)
- [Dokumentacja języka C++](https://en.cppreference.com/)
- [Przykłady z wykorzystaniem plików tekstowych](https://www.geeksforgeeks.org/working-with-files-cpp/)