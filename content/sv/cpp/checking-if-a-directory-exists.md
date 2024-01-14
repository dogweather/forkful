---
title:                "C++: Kontrollera om en katalog finns"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp finns är en viktig del av programmering eftersom det kan hjälpa dig att hantera dina filer och mappar på ett effektivt sätt. Om du till exempel försöker skapa en ny mapp men en mapp med samma namn redan finns, kan du använda funktionen för att undvika att överlappa eller skriva över befintliga filer.

## Hur man gör det

För att kontrollera om en mapp finns, kan du använda sig av funktionen "opendir". Genom att använda ```C++ opendir() ``` och sedan ```C++ readdir()``` kan du öppna en viss mapp och lista filerna i den.

Här är ett exempel på hur man kan använda-se ```C++ opendir()``` för att lista alla filer i en mapp:

```C++
#include <iostream>
#include <dirent.h>

using namespace std;

int main() {
    DIR *dir;
    struct dirent *ent;
    if ((dir = opendir ("C:\\Users\\ExampleUser\\Documents\\Test")) != NULL) {
        while ((ent = readdir (dir)) != NULL) {
            cout << ent->d_name << endl;
        }
        closedir (dir);
    } else {
        cout << "Kan inte öppna den här mappen" << endl;
    }
    return 0;
}
```
Efter att ha kört koden ovan, bör du få följande utdata:

```
file1.txt
file2.txt
file3.txt
file4.txt
```

## Djupdykning

Förutom att lista filer i en mapp, kan du också kontrollera om en specifik mapp finns genom att använda funktionen "opendir" kombinerat med en "if-sats". Om mappen finns, returnerar funktionen ett DIR-pekare och därmed evalueras "if-satsen" som sann. Om mappen inte hittas, returnerar funktionen NULL och "if-satsen" är inte sann.

En annan användbar funktion är "mkdir" som låter dig skapa en ny mapp om den inte redan finns. Här är ett exempel på hur man kan använda-se både "opendir" och "mkdir" för att kontrollera och skapa en mapp:

```C++
#include <iostream>
#include <dirent.h>
#include <sys/stat.h>

using namespace std;

int main() {
    DIR *dir;
    if ((dir = opendir ("C:\\Users\\ExampleUser\\Documents\\Test")) == NULL) {
        // Mappen finns inte, skapa en ny
        int result = mkdir("C:\\Users\\ExampleUser\\Documents\\Test", 0777);
        if (result == 0) {
            cout << "Mappen skapades!" << endl;
        } else {
            cout << "Det gick inte att skapa mappen." << endl;
        }
    } else {
        // Mappen finns redan
        cout << "Mappen finns redan." << endl;
    }
    return 0;
}
```

## Se även

- [Funktionen "opendir" i C++](https://en.cppreference.com/w/cpp/filesystem/opendir)
- [Funktionen "readdir" i C++](https://en.cppreference.com/w/cpp/filesystem/readdir)
- [Funktionen "mkdir" i C++](https://en.cppreference.com/w/cpp/filesystem/mkdir)