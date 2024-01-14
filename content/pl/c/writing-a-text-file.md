---
title:    "C: Pisanie pliku tekstowego"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie tekstowych plików jest jedną z podstawowych czynności w programowaniu w języku C. Jest to niezbędne dla przechowywania danych oraz komunikacji z użytkownikiem lub z innymi programami. Dzięki pisaniu plików tekstowych, możemy zapewnić trwałe przechowywanie informacji i łatwiejszą manipulację nimi.

## Jak to zrobić?

Pierwszym krokiem w pisaniu plików tekstowych jest otworzenie pliku za pomocą funkcji `fopen`. Należy również podać ścieżkę do pliku oraz tryb otwarcia, czyli czy chcemy tylko odczytywać plik, czy też go zapisywać.

```C
FILE *plik = fopen("nazwa_pliku.txt", "w");
```

Następnie możemy użyć funkcji `fprintf` do zapisania danych do pliku. W poniższym przykładzie zapisujemy do pliku tekst "Cześć wszystkim!".

```C
fprintf(plik, "Cześć wszystkim!");
```

Aby zamknąć plik i zapisać do niego zmiany, należy użyć funkcji `fclose`.

```C
fclose(plik);
```

## Dogłębna analiza

W języku C, pisanie plików tekstowych jest bardzo proste i opiera się na kilku podstawowych funkcjach. Możemy również wykorzystać funkcję `fscanf` do odczytywania danych z pliku lub `fgets` do odczytywania pojedynczej linii tekstu. Ważne jest również pamiętanie o poprawnym zamykaniu pliku po jego użyciu.

## Zobacz również

- Tutorial o obsłudze plików w języku C: [https://www.tutorialspoint.com/cprogramming/c_file_io.htm](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- Dokumentacja funkcji do obsługi plików w języku C: [https://www.tutorialspoint.com/c_standard_library/c_function_fclose.htm](https://www.tutorialspoint.com/c_standard_library/c_function_fclose.htm)
- Przykładowy kod pisania plików tekstowych w języku C: [https://www.geeksforgeeks.org/write-text-file-c/](https://www.geeksforgeeks.org/write-text-file-c/)