---
title:                "Lecture d'un fichier texte"
html_title:           "Fish Shell: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Lecture d'un fichier texte est simplement le fait de parcourir et de lire le contenu d'un fichier texte. Les programmeurs le font souvent pour extraire des données utiles ou pour manipuler des données dans un format spécifique.

## Comment faire:

```Fish Shell ...``` devrait être votre meilleur ami lorsque vous travaillez avec des fichiers texte. Voici quelques exemples de code pour vous montrer comment cela fonctionne:

#### Lire un fichier texte:
```
cat monfichier.txt
```

#### Lire un fichier texte ligne par ligne:
```
while read line
  echo $line
end < monfichier.txt
```

#### Afficher un aperçu d'un fichier texte:
```
head monfichier.txt
```

#### Copier le contenu d'un fichier texte dans un autre:
```
cp monfichier1.txt monfichier2.txt
```

## Plongée en profondeur:

Lecture de fichiers texte est une tâche courante dans la programmation et elle a été rendue encore plus facile avec les avancées en matière de langages de script comme Fish Shell. Avant, les programmeurs devaient utiliser des langages de programmation plus compliqués pour lire des fichiers texte, comme le C ou le Java. Maintenant, avec Fish Shell, il est beaucoup plus simple d'effectuer cette tâche. Bien sûr, il existe des alternatives telles que Python ou Perl, mais si vous travaillez souvent avec Fish Shell, cela peut être le choix le plus pratique.

## Voir aussi:
- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guide de la ligne de commande Fish](https://fishshell.com/docs/current/tutorial.html)
- [Lecture d'un fichier texte avec d'autres langages de programmation](https://www.tecmint.com/ways-to-read-a-file-line-by-line-in-shell-script/)