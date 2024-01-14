---
title:                "C++: Lesen von Befehlszeilenargumenten"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum

Beim Programmieren in C++ gibt es viele verschiedene Möglichkeiten, Werte in ein Programm zu übergeben. Eine der einfachsten und effizientesten Methoden ist die Verwendung von Kommandozeilenargumenten. Durch das Einfügen von Argumenten bei der Ausführung des Programms können wir bestimmte Werte an das Programm übergeben, ohne den Code jedes Mal ändern zu müssen. Nun fragen Sie sich vielleicht, wie man Kommandozeilenargumente in C++ liest und verwendet. In diesem Blogbeitrag werden wir darüber sprechen.

# Wie man Kommandozeilenargumente in C++ liest

Die Verwendung von Kommandozeilenargumenten in C++ ist sehr einfach. Alles, was wir tun müssen, ist, die  Argumente während der Programmausführung in das 'main()' Funktion aufzurufen. Hier ist ein Beispiel, wie Sie Ihr Programm in der Kommandozeile ausführen können, um 3 Argumente zu übergeben:

```C++
./programm argument1 argument2 argument3
```

Wenn wir nun die Argumente in unserem Code verwenden möchten, müssen wir sie in unserem 'main()' Funktion definieren. Dies kann durch Hinzufügen von zwei Parametern 'int argc' und 'char* argv[]' geschehen:

```C++
int main(int argc, char* argv[])
```

Jetzt können wir auf die Argumente zugreifen, indem wir auf das 'argv[]' Array zugreifen, das alle übergebenen Argumente enthält.

```C++
argv[0] // Programmname (hier "programm")
argv[1] // erstes Argument (hier "argument1")
argv[2] // zweites Argument (hier "argument2")
argv[3] // drittes Argument (hier "argument3")
```

# Tiefere Einblicke

Die Verwendung von Kommandozeilenargumenten ist nicht auf einfache Werte wie Strings oder Zahlen beschränkt. Wir können auch komplexe Datenstrukturen wie Arrays oder Strukturen als Argumente übergeben. Außerdem können wir optional Argumente definieren, indem wir sie in eckigen Klammern in der Programmausführung angeben. Wenn Sie mehr über die Verwendung von Kommandozeilenargumenten in C++ erfahren möchten, können Sie sich diese Ressourcen ansehen:

- [C++ Dokumentation - Kommandozeilenargumente](https://docs.microsoft.com/en-us/cpp/cpp/main-function-command-line-args?view=msvc-160)
- [GeeksforGeeks - Working with Command Line Arguments in C/C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Programiz - Command Line Arguments in C++](https://www.programiz.com/cpp-programming/command-line-arguments)

# Siehe auch

- [C++ Grundlagen für Anfänger](https://www.learn-c.org/)
- [C++ Referenzhandbuch](https://en.cppreference.com/w/)
- [Gute Programmiergewohnheiten und Richtlinien](https://www.freecodecamp.org/news/the-good-programmer-checklist-3f0584075ba2/)