---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:51.050863-07:00
description: "Capitaliser une cha\xEEne dans Bash consiste \xE0 transformer le premier\
  \ caract\xE8re de la cha\xEEne en majuscule tout en laissant le reste de la cha\xEE\
  ne inchang\xE9.\u2026"
lastmod: '2024-03-13T22:44:57.968046-06:00'
model: gpt-4-0125-preview
summary: "Capitaliser une cha\xEEne dans Bash consiste \xE0 transformer le premier\
  \ caract\xE8re de la cha\xEEne en majuscule tout en laissant le reste de la cha\xEE\
  ne inchang\xE9.\u2026"
title: "Mettre en majuscule une cha\xEEne"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Capitaliser une chaîne dans Bash consiste à transformer le premier caractère de la chaîne en majuscule tout en laissant le reste de la chaîne inchangé. Cette technique est couramment utilisée pour formater la sortie ou se conformer aux conventions de codage qui exigent que certaines chaînes commencent par une lettre capitale pour une meilleure lisibilité ou des préférences stylistiques.

## Comment faire :

Bash n'a pas de fonction intégrée spécifiquement pour la capitalisation des chaînes, mais vous pouvez accomplir cette tâche en utilisant l'expansion de paramètres ou des outils externes comme `awk`. Voici quelques façons de capitaliser une chaîne dans Bash :

**Utilisation de l'expansion de paramètres :**

Cette méthode manipule directement la chaîne dans le shell.

```bash
str="hello world"
capitalized="${str^}"
echo "$capitalized"
```
Sortie :
```
Hello world
```

**Utilisation de `awk` :**

`awk` est un outil puissant de traitement de texte disponible sur la plupart des systèmes d'exploitation de type Unix, qui peut être utilisé pour capitaliser les chaînes.

```bash
str="hello world"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
Sortie :
```
Hello world
```

**Utilisation de `sed` :**

Pour une approche plus traditionnelle, `sed` peut être employé pour capitaliser la première lettre d'une chaîne. Cependant, c'est un peu plus complexe par rapport aux méthodes précédentes.

```bash
str="hello world"
echo "$str" | sed 's/./\u&/'
```
Sortie :
```
Hello world
```

Ces extraits démontrent comment capitaliser la première lettre d'une chaîne dans Bash, soulignant la flexibilité du script shell lors de la manipulation de texte.
