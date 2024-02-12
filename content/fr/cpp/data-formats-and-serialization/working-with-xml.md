---
title:                "Travailler avec XML"
aliases:
- /fr/cpp/working-with-xml/
date:                  2024-01-26T04:28:23.396727-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/working-with-xml.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Travailler avec le XML signifie analyser, créer et manipuler des données XML (eXtensible Markup Language). Les programmeurs gèrent le XML pour traiter l'échange de données structurées, la configuration et plus encore, en raison de sa nature neutre sur la plateforme.

## Comment faire :
Voici une manière simple d'analyser le XML en utilisant la bibliothèque TinyXML-2 :

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Bonjour, le monde !</message></root>");
    const char* contenu = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << contenu << std::endl;
    return 0;
}
```

Exemple de sortie :

```
Bonjour, le monde !
```

Et voici comment vous créez un fichier XML :

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* declaration = doc.NewDeclaration();
    doc.InsertFirstChild(declaration);
    auto* racine = doc.NewElement("root");
    doc.InsertEndChild(racine);
    auto* message = doc.NewElement("message");
    message->SetText("Bonjour, le monde !");
    racine->InsertEndChild(message);
    doc.SaveFile("output.xml");
    return 0;
}
```

Ceci génère un fichier XML `output.xml` avec le contenu :

```xml
<?xml version="1.0"?>
<root>
    <message>Bonjour, le monde !</message>
</root>
```

## Exploration approfondie
Le XML a été essentiel dans les services web et le stockage de données depuis la fin des années 90. Alors que le JSON et le YAML sont maintenant plus courants pour la configuration et l'interopérabilité, le XML est toujours très utilisé dans de nombreux systèmes d'entreprise. Analyser le XML en C++ peut sembler vieux jeu avec l'analyse manuelle du DOM/SAX. Heureusement, des bibliothèques comme TinyXML-2 le simplifient. C++ ne dispose pas de support XML intégré ; des bibliothèques comme TinyXML-2, pugixml, ou Xerces enveloppent les parties difficiles.

## Voir Aussi
- Documentation TinyXML-2 : https://leethomason.github.io/tinyxml2/
- Bibliothèque pugixml : https://pugixml.org/
- Analyseur Xerces-C++ : https://xerces.apache.org/xerces-c/
- Spécification XML du W3C : https://www.w3.org/XML/
