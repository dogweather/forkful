---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:52.302471-07:00
description: "Les expressions r\xE9guli\xE8res (regex) dans Bash vous permettent de\
  \ rechercher, manipuler et g\xE9rer des cha\xEEnes de caract\xE8res et des fichiers\
  \ en fonction de\u2026"
lastmod: '2024-03-13T22:44:57.978651-06:00'
model: gpt-4-0125-preview
summary: "Les expressions r\xE9guli\xE8res (regex) dans Bash vous permettent de rechercher,\
  \ manipuler et g\xE9rer des cha\xEEnes de caract\xE8res et des fichiers en fonction\
  \ de\u2026"
title: "Utilisation des expressions r\xE9guli\xE8res"
weight: 11
---

## Quoi & Pourquoi ?

Les expressions régulières (regex) dans Bash vous permettent de rechercher, manipuler et gérer des chaînes de caractères et des fichiers en fonction de modèles spécifiques. Les programmeurs utilisent les regex pour des tâches telles que la validation d'entrée, l'analyse de fichiers journaux et l'extraction de données car elles offrent un moyen flexible et puissant de spécifier des modèles pour des besoins complexes de traitement de texte.

## Comment faire :

### Correspondance de modèles de base
Pour trouver si une chaîne correspond à un motif, vous pouvez utiliser `grep`, un utilitaire en ligne de commande pour rechercher dans des ensembles de données en texte brut des lignes correspondant à une expression régulière :

```bash
echo "Bonjour, le monde !" | grep -o "monde"
# Sortie : monde
```

### Extraction de données spécifiques
Pour extraire des parties de données qui correspondent à vos modèles regex, vous pouvez utiliser `-o` avec `grep` :

```bash
echo "Erreur : fichier non trouvé" | grep -oE "[A-Za-z]+:"
# Sortie : Erreur :
```

### Utilisation des Regex avec `sed`
`sed` (éditeur de flux) est un utilitaire puissant pour analyser et transformer du texte. Voici comment utiliser `sed` avec regex pour remplacer du texte :

```bash
echo "Bash est super" | sed -e 's/super/formidable/'
# Sortie : Bash est formidable
```

### Correspondance de modèles dans les instructions conditionnelles
Bash prend également en charge les regex dans les instructions conditionnelles directement :

```bash
[[ "https://exemple.com" =~ ^https?:// ]] && echo "L'URL est valide" || echo "L'URL est invalide"
# Sortie : L'URL est valide
```

### Correspondance de modèles avancée et manipulation avec `awk`
`awk` est un autre outil de traitement de texte qui prend en charge l'extraction et la manipulation de données plus complexes. Cela peut être bénéfique lorsque vous travaillez avec des données textuelles structurées, comme les CSV :

```bash
echo -e "ID,Nom,Âge\n1,John,22\n2,Jane,24" | awk -F, '$3 > 22 {print $2 " est plus âgé que 22."}'
# Sortie : Jane est plus âgé que 22.
```

Bien que les fonctionnalités regex intégrées de Bash couvrent de nombreux cas d'usage, pour des opérations regex très avancées, vous pourriez envisager d'utiliser une combinaison de scripts Bash avec des scripts `perl` ou `python`, car ces langues offrent des bibliothèques regex puissantes (par exemple, `re` en Python). Un exemple simple avec Python :

```bash
echo "Capturez cela 123" | python3 -c "import sys; import re; print(re.search('(\d+)', sys.stdin.read()).group(0))"
# Sortie : 123
```

Incorporer ces langages de programmation lorsque cela est nécessaire peut vous aider à exploiter toute la puissance des regex dans vos scripts Bash.
