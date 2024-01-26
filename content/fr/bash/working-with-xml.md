---
title:                "Travailler avec XML"
date:                  2024-01-26T04:27:30.326247-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec XML"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/working-with-xml.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Travailler avec XML implique d'analyser, d'extraire et de manipuler des données au format Extensible Markup Language. Les programmeurs se débattent avec XML car c'est un format d'échange de données répandu pour les configurations, les API, et plus encore.

## Comment faire :
Voici comment analyser XML en Bash. Les outils ? xmllint et xmlstarlet. Itérer à travers les éléments XML ? Absolument. Exemple avec sortie d'échantillon :

```bash
# En supposant que xmlstarlet est installé
# Installer avec : apt-get install xmlstarlet

# Analyse de contenu XML
cat <<EOF > sample.xml
<fruits>
  <fruit name="Pomme"/>
  <fruit name="Banane"/>
</fruits>
EOF

# Extraire les noms avec xmlstarlet
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# La sortie devrait être :
# Pomme
# Banane
```

## Plongée profonde
Retour dans les années 90, XML a émergé comme une alternative plus simple à SGML, mais plus structurée qu'HTML. Maintenant, il a de la compagnie - JSON, YAML, par exemple. Mais XML est toujours là, surtout dans les configurations et les services web basés sur SOAP.

Côté outils, xmllint est confortable pour la validation XML, les requêtes xpath. xmlstarlet est le couteau suisse pour les manigances XML - interroger, éditer, valider, transformer. Dans les scripts bash, ils sont les super-héros pour les tâches XML.

Sous le capot, xmllint utilise libxml2 – le parseur C XML. C'est rapide, mais les messages d'erreur ? Cryptiques. Et xmlstarlet ? Modèles récursifs et le support d’EXSLT. Casse-tête, mais puissant.

## Voir aussi
- [xmlsoft.org](http://xmlsoft.org/) : Trucs sur Libxml2 et xmllint.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash) : Problèmes et solutions du monde réel.
- [W3Schools Tutoriel XML](https://www.w3schools.com/xml/) : Bases de XML.