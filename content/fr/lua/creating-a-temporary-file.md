---
title:                "Création d'un fichier temporaire"
html_title:           "Lua: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La création d'un fichier temporaire est une technique couramment utilisée par les programmeurs pour stocker des données de manière temporaire. Cela peut être utile lors de la manipulation de fichiers ou de l'exécution de certaines opérations qui nécessitent un espace de stockage supplémentaire.

## Comment:
### Exemple 1:
```
**local** tempFile = io.open("temp.txt", "w")
tempFile:write("Contenu du fichier temporaire")
tempFile:close()
```
**Sortie:**
Un fichier temporaire nommé "temp.txt" sera créé et le texte "Contenu du fichier temporaire" y sera écrit.

### Exemple 2:
```
**local** tempFile = os.tmpname()
print(tempFile)
```
**Sortie:**
Un nom de fichier temporaire sera généré et affiché dans la console.

## Plongée en profondeur:
La création de fichiers temporaires est une pratique courante en programmation et remonte aux premières versions de Lua. Cela permet aux programmeurs de stocker temporairement des données sans impacter leur système de fichiers. Il existe également d'autres alternatives telles que l'utilisation de variables en mémoire, mais les fichiers temporaires sont souvent plus flexibles et peuvent être utilisés dans une variété de situations. En termes d'implémentation, Lua offre des fonctions natives pour créer et manipuler des fichiers, rendant la création de fichiers temporaires relativement simple.

## Voir aussi:
- [Documentation officielle de Lua sur les opérations de fichiers](https://www.lua.org/manual/5.4/manual.html#6.8)
- [Une alternative à la création de fichiers temporaires avec la bibliothèque "lfs"](https://github.com/keplerproject/luafilesystem)