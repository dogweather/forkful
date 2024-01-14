---
title:    "Arduino: Zapisywanie pliku tekstowego"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest jedną z podstawowych umiejętności potrzebnych przy programowaniu Arduino. Pozwala to na przechowywanie danych i informacji w postaci łatwej do odczytania dla ludzi, co jest niezbędne w wielu projektach.

## Jak to zrobić

Pisanie plików tekstowych w Arduino jest proste i opiera się na wykorzystaniu wbudowanej funkcji `File`. Możemy zacząć od zdefiniowania nazwy i trybu naszego pliku, na przykład:
```
Arduino File plik;
plik = SD.open("dane.txt", FILE_WRITE);
```
Następnie możemy użyć funkcji `println()` aby wypisać dane do pliku:
```
plik.println("To jest przykładowy tekst.");
```
Aby zapisać zmiany, należy użyć funkcji `close()`:
```
plik.close();
```

## Deep Dive

Aby zapewnić poprawne działanie naszego kodu, musimy mieć zainstalowaną bibliotekę `SD`. Następnie, w pętli głównej naszego programu, musimy wykonać dwa kroki. Pierwszym z nich jest sprawdzenie, czy karta SD jest poprawnie podłączona do naszego kontrolera:
```
if(!SD.begin(53)){
  Serial.println("Błąd podczas inicjalizacji karty SD.");
  return;
}
```
Następnie, wewnątrz pętli, możemy wywołać funkcję odpowiedzialną za zapis do pliku, jak w przykładzie podanym w poprzedniej sekcji.

## Zobacz również

- [Dokumentacja Arduino o pisaniu plików tekstowych](https://www.arduino.cc/en/Tutorial/WriteToFile)
- [Przykładowy projekt wykorzystujący pisanie plików tekstowych](https://create.arduino.cc/projecthub/Nicholas_N/piano-recording-with-data-storage-a1d709)