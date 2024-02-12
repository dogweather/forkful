---
title:                "Analyse Syntaxique du HTML"
aliases:
- fr/bash/parsing-html.md
date:                  2024-02-03T19:11:35.591596-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyse Syntaxique du HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

L'analyse du HTML implique de passer au crible la structure et le contenu d'un fichier HTML pour en extraire des informations. Les programmeurs le font pour accéder à des données, manipuler du contenu ou gratter des sites web.

## Comment faire :

Bash n'est pas l'outil de prédilection pour l'analyse du HTML, mais cela peut être réalisé avec des outils comme `grep`, `awk`, `sed`, ou des utilitaires externes comme `lynx`. Pour plus de robustesse, nous utiliserons `xmllint` du paquet `libxml2`.

```bash
# Installer xmllint si nécessaire
sudo apt-get install libxml2-utils

# Exemple de HTML
cat > sample.html <<EOF
<html>
<head>
  <title>Page Exemple</title>
</head>
<body>
  <h1>Bonjour, Bash !</h1>
  <p id="myPara">Bash peut me lire.</p>
</body>
</html>
EOF

# Analyser le Titre
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "Le titre est : $title"

# Extraire le Paragraphe par ID
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "Le contenu du paragraphe est : $para"
```

Sortie :
```
Le titre est : Page Exemple
Le contenu du paragraphe est : Bash peut me lire.
```

## Plongée en Profondeur

Autrefois, les programmeurs utilisaient des outils basés sur des expressions régulières comme `grep` pour scanner le HTML, mais c'était peu pratique. Le HTML n'est pas régulier, il est contextuel. Les outils traditionnels ne saisissent pas cela et peuvent être sujets à erreurs.

Des alternatives ? Beaucoup. Python avec Beautiful Soup, PHP avec DOMDocument, JavaScript avec des parseurs DOM—des langues avec des bibliothèques conçues pour comprendre la structure du HTML.

Utiliser `xmllint` dans les scripts bash est solide pour des tâches simples. Il comprend le XML, et par extension, le XHTML. Le HTML régulier peut être imprévisible, toutefois. Il ne suit pas toujours les règles strictes du XML. `xmllint` force le HTML dans un modèle XML, ce qui fonctionne bien pour du HTML bien formé, mais peut trébucher sur des choses désordonnées.

## Voir également

- [W3Schools - Analyseur DOM HTML](https://www.w3schools.com/xml/dom_intro.asp) : Démystifie le DOM HTML.
- [MDN Web Docs - Analyse et sérialisation du XML](https://developer.mozilla.org/fr/docs/Web/Guide/Parsing_and_serializing_XML) : Pour les principes d'analyse XML applicables au XHTML.
- [Documentation de Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/) : Une bibliothèque Python pour l'analyse du HTML.
- [Documentation de libxml2](http://xmlsoft.org/) : Détails sur `xmllint` et les outils XML associés.
