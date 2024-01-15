---
title:                "Eine Textdatei schreiben"
html_title:           "C++: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Das Schreiben von Textdateien ist eine grundlegende Fähigkeit in vielen Programmiersprachen, einschließlich C++. Es ermöglicht Programmierern, Daten in einer Datei zu speichern und später wieder auf sie zuzugreifen. Dies ist besonders nützlich, um große Datenmengen zu organisieren und zu verwalten.

# Wie geht's

Um eine Textdatei in C++ zu schreiben, müssen wir zuerst die Header-Datei `fstream` einbinden. Dann können wir ein Objekt der Klasse `ofstream` erstellen, das uns ermöglicht, die Datei zu öffnen und zu schreiben.

```C++
#include <fstream>

int main() {
  // Öffne eine Datei mit dem Namen "example.txt" im Schreibmodus
  std::ofstream datei("example.txt");
  
  // Schreibe eine Zeichenkette in die Datei
  datei << "Hallo, Welt!";
  
  // Schließe die Datei
  datei.close();
  
  return 0;
}
```

Um weitere Zeilen in die Datei zu schreiben, können wir die `<<` Operatorüberladung weiter verwenden oder die `write()` Funktion verwenden.

```C++
// Weitere Zeilen schreiben
datei << "Dies ist eine weitere Zeile.";
datei.write("Noch eine Zeile!", 16);  // Schreibt die ersten 16 Charaktere der Zeichenkette
```

Nachdem wir die Datei geschrieben haben, müssen wir sicherstellen, dass wir sie schließen, um sie zu speichern und Ressourcen freizugeben.

# Tiefer eintauchen

Um eine Textdatei effizienter zu schreiben, können wir die Datei im Binärmodus öffnen und die `write()` Funktion verwenden, um ganze Datenstrukturen zu schreiben. Dies ist nützlich, wenn wir beispielsweise eine Klasse haben, die wir in der Datei speichern möchten.

```C++
#include <fstream>

class Person {
  private:
    std::string name;
    int alter;
    
  public:
    // Konstruktor, Getter und Setter
};

int main() {
  // Öffne eine Datei im Binärmodus
  std::ofstream datei("personen.txt", std::ios::binary);
  
  // Erstelle ein Objekt der Klasse Person
  Person person("Max", 25);
  
  // Schreibe das Objekt in die Datei
  datei.write((char*)&person, sizeof(person));
  
  // Schließe die Datei
  datei.close();
  
  return 0;
}
```

Dies zeigt, wie wir komplexe Datenstrukturen in einer Textdatei speichern können, anstatt nur Zeichenketten oder einfache Datentypen. Wir können dann die `read()` Funktion verwenden, um die Daten aus der Datei wiederherzustellen.

# Siehe auch

- [Ein Blick auf die verschiedenen Dateimodi in C++](https://www.geeksforgeeks.org/different-modes-open-files-c/)
- [C++-Referenz für die Header-Datei `fstream`](https://www.cplusplus.com/reference/fstream/)