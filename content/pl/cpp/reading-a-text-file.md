---
title:    "C++: Odczytywanie pliku tekstowego"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub utrzymujesz bloga, możliwe że pewnego dnia będziesz musiał czytać plik tekstowy, aby przetworzyć jego zawartość. W tym poście dowiesz się jak to zrobić za pomocą języka C++.

## Jak to zrobić

Pierwszym krokiem jest otworzenie pliku tekstowego za pomocą funkcji `ifstream` i przekazanie nazwy pliku jako argumentu. Można to zrobić w następujący sposób:

```C++
ifstream plik("tekst.txt");
```

Kolejnym krokiem jest sprawdzenie, czy plik został otwarty poprawnie. Można to zrobić wykorzystując funkcję `is_open()`. Jeśli plik został otwarty prawidłowo, możemy kontynuować dalsze operacje na nim. Na przykład:

```C++
if(plik.is_open()){
    // wykonywanie operacji na pliku
}
```

Teraz możemy zacząć odczytywać zawartość pliku. Możemy użyć pętli `while` do czytania plików wiersz po wierszu za pomocą funkcji `getline()`:

```C++
string wiersz;
while(getline(plik, wiersz)){
    // wykonanie operacji na wierszu
    cout << wiersz << endl;
}
```

Możemy również odczytywać zawartość pliku znak po znaku, używając funkcji `get()`:

```C++
char znak;
while(plik.get(znak)){
    // wykonanie operacji na znaku
    cout << znak;
}
```

Po zakończeniu operacji na pliku, należy go zamknąć wywołując funkcję `close()`:

```C++
plik.close();
```

Aby mieć pewność, że plik został zamknięty prawidłowo, możemy sprawdzić to za pomocą funkcji `is_open()`:

```C++
if(plik.is_open()){
    cout << "Plik nie został zamknięty";
}
```

## Głębsza analiza

W języku C++ istnieje wiele innych możliwości odczytu pliku tekstowego, takich jak funkcje `get()`, `putback()`, `tellg()`, `seekg()`, które pozwalają na bardziej precyzyjne manipulowanie zawartością pliku. Dodatkowo, istnieją wbudowane funkcje do konwersji typów danych, co ułatwia przetwarzanie zawartości pliku.

Pamiętaj jednak, że przed przystąpieniem do operacji na plikach należy sprawdzić, czy zostały zawarte one prawidłowo. W przeciwnym razie może to spowodować błędne wyniki lub awarię programu.

## Zobacz również

- [Dokumentacja języka C++ na temat odczytu plików](https://en.cppreference.com/w/cpp/io/basic_ifstream)
- [Przykładowy kod na odczyt pliku tekstowego w C++](https://www.learncpp.com/cpp-tutorial/186-basic-file-io/)