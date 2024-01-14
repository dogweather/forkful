---
title:    "Bash: Concaténer des chaînes de caractères"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes lors de la programmation est la concaténation de chaînes de caractères. Cela consiste à combiner plusieurs chaînes en une seule et peut sembler simple, mais il peut être utile de savoir comment le faire correctement. Dans cet article, nous allons plonger dans le monde de la concaténation de chaînes en utilisant Bash.

## Comment faire

La concaténation de chaînes en Bash peut être réalisée de différentes manières, en fonction de vos besoins spécifiques. Voici quelques exemples de code pour montrer comment cela peut être fait:

```
# Concaténer deux chaînes simples
nom='John'
nom_complet=$nom'Doe'
echo $nom_complet # Résultat: John Doe

# Concaténer des chaînes avec une variable
prenom='Jane'
nom_complet="$prenom $nom"
echo $nom_complet # Résultat: Jane Doe

# Concaténer une chaîne avec du texte statique
echo "Bienvenue $prenom" # Résultat: Bienvenue Jane
```

Comme vous pouvez le voir, il est possible de concaténer des chaînes de différentes manières en utilisant des variables et du texte statique. Il est important de noter que lors de l'utilisation de variables, il est nécessaire d'inclure les guillemets doubles pour s'assurer que les espaces sont gérés correctement.

## Plongée profonde

Si vous voulez aller plus loin dans la concaténation de chaînes en Bash, il est utile de comprendre comment cela fonctionne en arrière-plan. En réalité, la concaténation en Bash est en fait une forme de substitution de variables, où les variables sont remplies et évaluées pour créer une nouvelle chaîne combinée.

De plus, Bash offre également certaines fonctionnalités avancées pour la concaténation de chaînes, comme l'utilisation de l'opérateur `+=` pour ajouter du texte à une chaîne existante, ou l'utilisation de la commande `printf` pour formater les chaînes de manière plus complexe.

## Voir aussi

- [Guide Bash pour les débutants](https://linuxize.com/post/bash-scripting-beginners-guide/)
- [Documentation officielle de Bash](https://www.gnu.org/software/bash/)
- [Exemples de code de concaténation de chaînes en Bash](https://www.lifewire.com/concatenate-strings-bash-2202054)