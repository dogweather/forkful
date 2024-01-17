---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Java: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Tworzenie pliku tymczasowego jest częstym zadaniem dla programistów. Polega ono na tworzeniu tymczasowych plików, które są potrzebne do wykonania określonych operacji lub do przechowywania danych, które nie są potrzebne na stałe. Najczęstszym powodem, dla którego programiści tworzą pliki tymczasowe, jest potrzeba przechowywania danych podczas wykonywania programu.

## Jak to zrobić:

```java
// Tworzenie pliku tymczasowego i zapisywanie do niego tekstu
File tempFile = File.createTempFile("temp", ".txt");
Writer writer = new BufferedWriter(new FileWriter(tempFile));
writer.write("To jest przykładowy tekst w pliku tymczasowym");
writer.close();

// Odczytywanie danych z pliku tymczasowego
Reader reader = new FileReader(tempFile);
System.out.println(reader.read());
reader.close();
```

**Output:**
``` 
To jest przykładowy tekst w pliku tymczasowym
```

## Głębsze zagadnienia:

Tworzenie pliku tymczasowego jest powszechną praktyką w programowaniu. Istnieje wiele powodów, dla których programiści mogą chcieć stworzyć tymczasowy plik, na przykład:

- Przechowywanie danych wykorzystywanych tylko w trakcie wykonywania programu, które nie są potrzebne na stałe.
- Utworzenie bufora w celu przechowywania informacji w trakcie przetwarzania danych.
- Przygotowanie pliku tymczasowego do zapisu lub odczytu danych z serwera lub innego urządzenia.

Alternatywnym sposobem tworzenia tymczasowych plików jest użycie metody `File.createTempFile()`, jednak nie jest to zalecane, ponieważ zawiera ona pewne wady. Należy pamiętać, że plik tymczasowy może zostać usunięty przez system operacyjny w dowolnym momencie, więc należy go odpowiednio obsłużyć w kodzie programu.

## Zobacz też:

- [Dokumentacja Java SE - Tworzenie pliku tymczasowego](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Poradnik dla programistów Java - Przechowywanie danych w plikach tymczasowych](https://www.baeldung.com/java-temporary-file)