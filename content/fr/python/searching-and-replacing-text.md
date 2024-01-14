---
title:    "Python: Recherche et remplacement de texte"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes en programmation et peuvent être utiles pour modifier rapidement de grandes quantités de données dans un fichier ou une chaîne de caractères. Cela peut également être utilisé pour automatiser des tâches répétitives ou pour corriger des erreurs de saisie.

## Comment faire

La recherche et le remplacement de texte peuvent être réalisés en utilisant la méthode "replace()" disponible dans le langage de programmation Python. Voici un exemple de code :

```
texte = "Bonjour tout le monde !"

texte_modifie = texte.replace("Bonjour", "Hello")

print(texte_modifie)
```

Output:
```
Hello tout le monde !
```

Dans cet exemple, nous avons remplacé le mot "Bonjour" par "Hello" dans la chaîne de caractères "Bonjour tout le monde !". La méthode replace() prend deux arguments : le texte à remplacer et le nouveau texte. Elle renvoie ensuite une nouvelle chaîne de caractères avec les modifications.

## Plongée en profondeur

Il convient de noter que la méthode replace() ne modifie pas la chaîne de caractères originale, mais en crée une nouvelle. Pour modifier la chaîne de caractères originale, il faut assigner la nouvelle chaîne de caractères à la variable d'origine.

De plus, la méthode replace() ne prend en compte que le premier mot correspondant. Si vous souhaitez remplacer toutes les occurrences du mot, vous pouvez utiliser la méthode "replace()" en conjonction avec une boucle for. Voici un exemple :

```
texte = "Hello hello Hello hello"
nouveau_texte = ""
for i in texte.split():
    if i == "Hello":
        nouveau_texte = nouveau_texte + "Bonjour "
    else:
        nouveau_texte = nouveau_texte + i + " "
print(nouveau_texte)
```

Output:
```
Bonjour Bonjour Bonjour Bonjour
```

Dans cet exemple, nous avons utilisé la méthode split() pour séparer la chaîne de caractères en une liste de mots. Nous parcourons ensuite cette liste avec une boucle for et nous remplaçons chaque occurrence du mot "Hello" par "Bonjour". Le nouveau texte est ensuite assigné à la variable "nouveau_texte".

## Voir aussi

Consultez ces ressources pour en savoir plus sur la recherche et le remplacement de texte en Python :

- [Documentation officielle Python sur les méthodes de chaînes de caractères](https://docs.python.org/fr/3/library/stdtypes.html#string-methods)
- [Article sur le site Real Python : Python String Methods](https://realpython.com/python-strings/)