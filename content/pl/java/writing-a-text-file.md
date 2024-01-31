---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Tworzenie pliku tekstowego w Java oznacza zapisanie danych do pliku, który łatwo odczytać i edytować. Programiści to robią, aby zapisywać ustawienia, logi i wymieniane przez aplikację dane.

## How to:

```Java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        String text = "Witaj, świecie!";
        try (BufferedWriter writer = new BufferedWriter(new FileWriter("przyklad.txt"))) {
            writer.write(text);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Efekt:
Tworzy plik `przyklad.txt` z tekstem "Witaj, świecie!".

## Deep Dive

Pisanie do plików tekstowych w Javie jest ważną częścią programowania - umożliwia zapisywanie danych na trwałe. Historycznie kożystano z `FileWriter`, ale w celu wydajności dodano `BufferedWriter`. Poza tym, od Javy 7, istnieje `Files.write`, oferujące prostszą alternatywę.
Zapisywanie plików ma kluczowe znaczenie dla operacji I/O. Pojęcia takie jak strumienie, bufforowanie i kodowanie znaków są warte zrozumienia. Gdy `BufferedWriter` jest używany z `try-with-resources`, zasoby są automatycznie zwalniane, co jest dobrym zwyczajem, zapobiegającym wyciekom pamięci.

## See Also

- Dokumentacja klasy `FileWriter`: https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html
- Dokumentacja klasy `BufferedWriter`: https://docs.oracle.com/javase/8/docs/api/java/io/BufferedWriter.html
- Przewodnik Oracle po I/O: https://docs.oracle.com/javase/tutorial/essential/io/
- Wprowadzenie do NIO.2 (nowszego API plików w Javie): https://docs.oracle.com/javase/tutorial/essential/io/fileio.html
