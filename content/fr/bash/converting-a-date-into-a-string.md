---
title:                "Bash: Conversion d'une date en chaîne de caractères"
simple_title:         "Conversion d'une date en chaîne de caractères"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes habitué à travailler avec des dates dans votre code Bash, vous savez sans doute à quel point il peut être frustrant de devoir manipuler ces données sous forme de chaînes de caractères. Heureusement, il existe des moyens simples de convertir une date en une chaîne de caractères pour faciliter la manipulation et l'affichage.

## Comment faire
Tout d'abord, vous devez déterminer le format de date que vous souhaitez utiliser dans votre chaîne de caractères. Par exemple, vous pouvez choisir d'afficher la date au format "jour/mois/année", "mois/jour/année" ou "année-mois-jour". Une fois que vous avez déterminé votre format, vous pouvez utiliser la commande `date` avec l'option `-d` pour spécifier la date que vous souhaitez convertir et l'option `+` pour indiquer le format de sortie.

```Bash
date -d "12 Dec 2021" +"%d/%m/%Y"
```
Sortie: 12/12/2021

Si vous souhaitez incorporer la date dans une chaîne de caractères plus complexe, vous pouvez utiliser la substitution de commandes avec la syntaxe `$(command)` pour inclure la sortie de la commande `date` dans votre chaîne. Par exemple, si vous voulez créer un fichier avec la date actuelle dans son nom, vous pouvez utiliser une commande comme celle-ci :

```Bash
touch "fichier_$(date +"%Y%m%d").txt"
```
Sortie: fichier_20211212.txt

Vous pouvez également utiliser les commandes `cut` et `echo` pour manipuler la chaîne de sortie de la commande `date`. Par exemple, si vous souhaitez récupérer uniquement le mois et l'année actuels, vous pouvez utiliser cette commande :

```Bash
echo $(date +"%m-%Y" | cut -d'-' -f2)
```
Sortie: 12-2021

## Plongée en profondeur
La commande `date` est basée sur le langage de programmation GNU C, qui utilise le concept de spécificateurs de conversion pour indiquer le format des dates. Les spécificateurs de conversion les plus couramment utilisés pour convertir une date en une chaîne de caractères sont :

- `%Y` : année sur quatre chiffres
- `%m` : mois sur deux chiffres
- `%d` : jour sur deux chiffres
- `%H` : heure sur 24 heures sur deux chiffres
- `%M` : minutes sur deux chiffres
- `%S` : secondes sur deux chiffres

Vous pouvez utiliser ces spécificateurs de conversion dans n'importe quel ordre et avec des caractères supplémentaires pour créer un format de date personnalisé.

## Voir aussi
- [Documentation sur la commande date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Guide sur les spécificateurs de conversion de date en C](https://www.gnu.org/software/libc/manual/html_node/Formatting-Data-Time-Strings.html)