---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:15.272345-07:00
description: "Comment faire : C++ offre plusieurs moyens d'\xE9crire dans un fichier\
  \ texte, mais l'une des m\xE9thodes les plus directes est d'utiliser la biblioth\xE8\
  que\u2026"
lastmod: '2024-03-13T22:44:58.186663-06:00'
model: gpt-4-0125-preview
summary: "C++ offre plusieurs moyens d'\xE9crire dans un fichier texte, mais l'une\
  \ des m\xE9thodes les plus directes est d'utiliser la biblioth\xE8que `<fstream>`\
  \ qui fournit la classe `ofstream` (flux de fichier de sortie) con\xE7ue pour les\
  \ op\xE9rations d'\xE9criture de fichiers."
title: "R\xE9diger un fichier texte"
weight: 24
---

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
