---
title:                "C++: Überprüfen, ob ein Verzeichnis existiert"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##Warum
Beim Programmieren kann es oft vorkommen, dass man überprüfen muss, ob ein bestimmtes Verzeichnis existiert. Dies kann zum Beispiel beim Speichern von Daten oder beim Zugriff auf Dateien wichtig sein. In diesem Blogpost werde ich erklären, wie man in C++ überprüfen kann, ob ein Verzeichnis existiert und was dabei zu beachten ist.

##Wie geht das?
Um zu überprüfen, ob ein Verzeichnis existiert, verwenden wir die "filesystem" Bibliothek von C++. Diese wurde in C++17 eingeführt und bietet viele nützliche Funktionen für die Arbeit mit Dateien und Verzeichnissen.

Um diese Bibliothek in unserem Code zu nutzen, müssen wir sie zunächst im Header-Teil unseres Programms mit `#include <filesystem>` einbinden.

Als nächstes benötigen wir eine Variable vom Typ `std::filesystem::path`, die den Pfad zum Verzeichnis, welches wir überprüfen möchten, enthält. Dieser Pfad kann entweder absolut oder relativ sein. Mit folgendem Code können wir überprüfen, ob das Verzeichnis existiert:

```
#include <filesystem>

// Pfad zum Verzeichnis festlegen
std::filesystem::path meinVerzeichnis("/home/benutzer/Dokumente");

// Überprüfen, ob das Verzeichnis existiert
if (std::filesystem::exists(meinVerzeichnis)) {
    std::cout << "Das Verzeichnis existiert!" << std::endl;
} else {
    std::cout << "Das Verzeichnis existiert nicht!" << std::endl;
}
```

Die Funktion `std::filesystem::exists()` gibt true zurück, wenn das Verzeichnis existiert, ansonsten wird false zurückgegeben. Wir können also anhand des Rückgabewertes entscheiden, wie wir weiter vorgehen wollen.

Manchmal reicht es nicht aus, nur zu überprüfen, ob ein Verzeichnis existiert. Wir möchten vielleicht auch wissen, ob es sich dabei tatsächlich um ein Verzeichnis handelt und nicht um eine Datei. Dafür können wir die Funktion `std::filesystem::is_directory()` verwenden. Diese gibt ebenfalls true oder false zurück und kann zusammen mit `std::filesystem::exists()` genutzt werden.

```
// Überprüfen, ob das Verzeichnis existiert und ob es sich dabei um ein Verzeichnis handelt
if (std::filesystem::exists(meinVerzeichnis) && std::filesystem::is_directory(meinVerzeichnis)) {
    // Hier können wir z.B. auf das Verzeichnis zugreifen oder weitere Aktionen ausführen
}
```

##Tiefere Einblicke
Wenn wir `#include <filesystem>` in unserem Code verwenden, wird die gesamte "filesystem" Bibliothek eingebunden. Das kann allerdings zu Problemen führen, zum Beispiel wenn wir auch andere Bibliotheken einbinden, die ebenfalls Datei- und Verzeichnis-Funktionen anbieten.

Um dies zu vermeiden, können wir die einzelnen Funktionen gezielt einbinden, indem wir `#include <filesystem>` durch den gewünschten Funktionsaufruf ersetzen. Zum Beispiel:

```
// Nur die Funktionen exists() und is_directory() einbinden
#include <filesystem>
using std::filesystem::exists;
using std::filesystem::is_directory;
```

Das gibt uns mehr Kontrolle darüber, welche Funktionen wir verwenden und vermeidet potenzielle Konflikte mit anderen Bibliotheken.

##Siehe auch
- [Offizielle Dokumentation zur "filesystem" Bibliothek](https://en.cppreference.com/w/cpp/filesystem)
- [Übersicht über nützliche C++17 Funktionen](https://www.heise.de/select/ct/2018/25/1543912245472471) (auf Deutsch)
- [Artikel zur Verwendung von C++17 Bibliotheken in älteren Versionen](https://abseil.io/blog/20200313-filesystem-detection) (auf Englisch)