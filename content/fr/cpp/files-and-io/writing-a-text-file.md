---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:15.272345-07:00
description: "\xC9crire dans un fichier texte en C++ implique de cr\xE9er ou d'ouvrir\
  \ un fichier, puis d'y \xE9crire des donn\xE9es, ce qui est une t\xE2che fondamentale\
  \ pour les\u2026"
lastmod: '2024-02-25T18:49:54.842230-07:00'
model: gpt-4-0125-preview
summary: "\xC9crire dans un fichier texte en C++ implique de cr\xE9er ou d'ouvrir\
  \ un fichier, puis d'y \xE9crire des donn\xE9es, ce qui est une t\xE2che fondamentale\
  \ pour les\u2026"
title: "R\xE9diger un fichier texte"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Écrire dans un fichier texte en C++ implique de créer ou d'ouvrir un fichier, puis d'y écrire des données, ce qui est une tâche fondamentale pour les applications nécessitant de conserver des données, telles que des journaux, du contenu généré par les utilisateurs ou des paramètres de configuration. Les programmeurs font cela pour sauvegarder les données générées pendant l'exécution d'un programme ou pour exporter des données à utiliser par d'autres programmes ou utilisateurs.

## Comment faire :
C++ offre plusieurs moyens d'écrire dans un fichier texte, mais l'une des méthodes les plus directes est d'utiliser la bibliothèque `<fstream>` qui fournit la classe `ofstream` (flux de fichier de sortie) conçue pour les opérations d'écriture de fichiers.

### Exemple utilisant `<fstream>` :

```cpp
#include <fstream>
#include <iostream>

int main() {
    std::ofstream file("exemple.txt");
    if (file.is_open()) {
        file << "Bonjour, monde !\n";
        file << "Écrire dans un fichier en C++ est simple.";
        file.close();
    } else {
        std::cerr << "Échec de l'ouverture du fichier\n";
    }
    return 0;
}
```

**Exemple de sortie dans 'exemple.txt':**
```
Bonjour, monde !
Écrire dans un fichier en C++ est simple.
```

Lorsque les données sont plus complexes ou qu'un contrôle plus poussé du processus d'écriture est nécessaire, les programmeurs peuvent se tourner vers des bibliothèques tierces telles que Boost Filesystem.

### Exemple utilisant Boost Filesystem :

Pour utiliser Boost pour les opérations sur fichier, vous devez d'abord installer les bibliothèques Boost. L'exemple suivant montre comment créer et écrire dans un fichier en utilisant `boost::filesystem` et `boost::iostreams`.

```cpp
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/stream.hpp>
#include <iostream>

namespace io = boost::iostreams;
namespace fs = boost::filesystem;

int main() {
    fs::path filePath("boost_exemple.txt");
    io::stream_buffer<io::file_sink> buf(filePath.string());
    std::ostream out(&buf);
    out << "Boost rend les opérations sur fichier faciles.\n";
    out << "Ceci est une ligne écrite avec Boost.";
    
    return 0;
}
```

**Exemple de sortie dans 'boost_exemple.txt':**
```
Boost rend les opérations sur fichier faciles.
Ceci est une ligne écrite avec Boost.
```

Le choix entre le C++ brut et une bibliothèque tierce comme Boost peut dépendre des besoins spécifiques de votre projet et du niveau de contrôle ou de flexibilité que vous nécessitez sur les opérations d'entrée/sortie de fichiers.
