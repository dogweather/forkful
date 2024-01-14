---
title:    "Python: Tworzenie pliku tekstowego"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Dlaczego

Programowanie to nie tylko wykonywanie zadań, ale również tworzenie czegoś nowego i przydatnego. Pisanie plików tekstowych jest ważną częścią codziennych obowiązków programisty, ponieważ umożliwia zapisywanie danych i informacji w czytelnej formie. Pozwala to na wykorzystanie tych plików do przechowywania ustawień, generowania raportów i wiele innych. 

## Jak to zrobić

Aby zapisać dane do pliku tekstowego w języku Python, możemy skorzystać z wbudowanej funkcji "open". Przykładowy kod wyglądałby następująco:

```python
file = open("plik_tekstowy.txt", "w") # tworzymy obiekt pliku
file.write("To jest tekst, który zostanie zapisany w pliku") # używamy funkcji write do zapisania tekstu
file.close() # zamykamy plik
```

Aby odczytać zawartość pliku tekstowego, możemy użyć funkcji "read":

```python
file = open("plik_tekstowy.txt", "r") # otwieramy plik w trybie odczytu
content = file.read() # odczytujemy zawartość pliku i przypisujemy do zmiennej content
print(content) # wyświetlamy zawartość pliku
file.close() # zamykamy plik
```

W ten sposób możemy również wykorzystać pętle, aby odczytać plik linijka po linijce:

```python
file = open("plik_tekstowy.txt", "r")
for line in file:
    print(line) # wyświetlamy każdą linijkę pliku
file.close()
```

## Głębszy wgląd

Podczas pisania plików tekstowych, istnieje kilka innych funkcji, które mogą być przydatne, takich jak "append" do dodawania tekstu do istniejącego pliku lub "readline" do odczytywania jednej linii pliku na raz. Możemy również wykorzystać blok "with open", który automatycznie zamyka plik po wyjściu z niego. 

Pamiętaj, żeby zawsze upewnić się, że plik został poprawnie zamknięty po zakończeniu operacji, aby uniknąć problemów z dostępem do pliku w przyszłości.

## Zobacz także

- [Dokumentacja Pythona o pisanu plików](https://docs.python.org/pl/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Poradnik o manipulacji plikami tekstowymi w Pythonie](https://www.digitalocean.com/community/tutorials/how-to-handle-plain-text-files-in-python-3)
- [Przykłady kodu do manipulacji plikami tekstowymi w Pythonie](https://www.programiz.com/python-programming/file-operation)