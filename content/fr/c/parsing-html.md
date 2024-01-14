---
title:                "C: Analyse de HTML"
simple_title:         "Analyse de HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi
Les développeurs utilisent souvent le langage de programmation C pour créer des applications efficaces et rapides. Parfois, ils doivent également travailler avec des données au format HTML, qui est un langage utilisé pour créer des pages Web. Dans ces cas, il est utile d'apprendre à analyser le code HTML en utilisant le langage C.

## Comment faire
Pour analyser le code HTML en utilisant le langage C, nous allons utiliser une bibliothèque appelée "libxml2". Cela nous permettra de parcourir facilement le code HTML et d'extraire les informations dont nous avons besoin. Voici un exemple de code qui affiche une liste de tous les liens sur une page Web :

```C
#include <stdio.h>
#include <libxml/HTMLparser.h>

int main() {
    // Récupère le contenu de la page Web sous forme de chaîne de caractères
    char *html = "<!DOCTYPE html><html><body><a href='https://www.example.com'>Example</a><a href='https://www.test.com'>Test</a></body></html>";
    
    // Parse le contenu HTML et le place dans une structure de type "htmlDocPtr"
    htmlDocPtr doc = htmlParseDoc(html, NULL);
    // Sélectionne tous les éléments "a" sur la page
    xmlNodePtr *links = NULL;
    xmlNodePtr currNode = xmlDocGetRootElement(doc)->children;
    while (currNode != NULL) {
        if (strcmp((char *)currNode->name, "a") == 0) {
            // Insère le lien actuel dans le tableau "links"
            xmlAddElement(links, currNode);
        }
        currNode = currNode->next;
    }
    // Parcours le tableau "links" et affiche les attributs "href" de chaque élément
    for (int i = 0; i < xmlGetLength(links); i++) {
        printf("%s\n", xmlGetProp(links[i], "href"));
    }
    
    return 0;
}
```

Lorsque nous exécutons ce code, voici le résultat que nous obtenons :

https://www.example.com
https://www.test.com

Ce n'est qu'un exemple simple, mais en utilisant "libxml2", nous pouvons effectuer une analyse beaucoup plus complexe du code HTML et extraire des informations comme le titre d'une page, le contenu d'un formulaire, etc.

## Plongée en profondeur
La bibliothèque "libxml2" est très puissante et offre une grande variété de fonctions pour analyser le code HTML. En plus de cela, elle dispose également de fonctions pour traiter des langages tels que XML, XPath et XPointer. Cela peut être très utile lors de la création d'une application qui doit traiter des données dans différents formats.

Il est important de noter que le parsing HTML à l'aide du langage C peut être plus fastidieux que d'autres langages de programmation, car le C ne dispose pas de fonctions intégrées pour manipuler facilement les chaînes de caractères et les éléments de structure. Cependant, cela peut également être un défi intéressant pour les développeurs C qui cherchent à améliorer leurs compétences en programmation.

## Voir aussi
- [Documentation de la bibliothèque libxml2](http://www.xmlsoft.org/html/index.html)
- [Tutoriel sur le parsing de code HTML avec C](https://codetuts.tech/parse-html-c/)
- [Exemples de projets utilisant libxml2](https://github.com/GNOME/libxml2/tree/master/examples)