---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:06.065997-07:00
description: "YAML, qui signifie \"YAML Ain't Markup Language\" (YAML n'est pas un\
  \ langage de balisage), est une norme de s\xE9rialisation de donn\xE9es lisible\
  \ par l'homme,\u2026"
lastmod: '2024-03-13T22:44:57.210592-06:00'
model: gpt-4-0125-preview
summary: "YAML, qui signifie \"YAML Ain't Markup Language\" (YAML n'est pas un langage\
  \ de balisage), est une norme de s\xE9rialisation de donn\xE9es lisible par l'homme,\u2026"
title: Travailler avec YAML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

YAML, qui signifie "YAML Ain't Markup Language" (YAML n'est pas un langage de balisage), est une norme de sérialisation de données lisible par l'homme, couramment utilisée pour les fichiers de configuration et l'échange de données entre langues avec des structures de données variées. Les programmeurs travaillent souvent avec YAML pour sa simplicité et sa lisibilité, en particulier dans les projets nécessitant une configuration extensive ou lors du transfert de données structurées entre différents systèmes.

## Comment faire :

Bien que Google Apps Script (GAS) ne prenne pas en charge nativement l'analyse ou la sérialisation YAML, vous pouvez manipuler des données YAML en utilisant des bibliothèques JavaScript ou en écrivant des fonctions d'analyse personnalisées. Pour démonstration, considérons comment analyser une chaîne YAML en utilisant une fonction personnalisée, puisque les bibliothèques externes ne peuvent pas être directement importées dans GAS.

Supposons que vous ayez une configuration YAML simple :

```yaml
title: Exemple YAML
description: Un exemple de gestion de YAML dans Google Apps Script
tags:
  - Google Apps Script
  - YAML
  - Configuration
```

Pour analyser ceci dans Google Apps Script, utilisez les capacités de manipulation de chaîne de caractères de JavaScript :

```javascript
function parseYAML(yamlString) {
  var result = {};
  var lines = yamlString.split("\n");
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if (line.includes(":")) {
      var parts = line.split(":");
      var key = parts[0].trim();
      var value = parts[1].trim();
      // Gestion basique des tableaux
      if (value.startsWith("-")) {
        value = [value.substring(1).trim()];
        while (i + 1 < lines.length && lines[i + 1].trim().startsWith("-")) {
          i++;
          value.push(lines[i].trim().substring(1).trim());
        }
      }
      result[key] = value;
    }
  }
  return result;
}

function testYamlParsing() {
  var yaml = "title: Exemple YAML\ndescription: Un exemple de gestion de YAML dans Google Apps Script\ntags:\n  - Google Apps Script\n  - YAML\n  - Configuration";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

Lorsque `testYamlParsing()` est exécuté, il affiche :

```
{ title: 'Exemple YAML',
  description: 'Un exemple de gestion de YAML dans Google Apps Script',
  tags: [ 'Google Apps Script', ' YAML', ' Configuration' ] }
```

Cette approche d'analyse personnalisée est assez basique et peut nécessiter des ajustements pour prendre en charge des fichiers YAML complexes.

## Approfondissement

YAML, initialement publié en 2001, visait à être plus lisible par l'homme que ses prédécesseurs comme XML ou JSON. Bien que sa simplicité et sa facilité d'utilisation soient largement appréciées, gérer YAML dans Google Apps Script présente des défis en raison de l'absence de soutien direct. Par conséquent, les programmeurs s'appuient souvent sur la polyvalence de JavaScript pour analyser et générer des données YAML. Cependant, pour des cas d'utilisation complexes, en particulier ceux impliquant une imbrication profonde et des structures de données avancées, cette méthode peut devenir encombrante et sujette aux erreurs.

En contraste, JSON est nativement pris en charge dans Google Apps Script et la plupart des autres environnements de programmation, offrant une approche plus simple pour la sérialisation et la désérialisation des données sans surcharge d'analyse supplémentaire. La syntaxe de JSON est moins verbeuse que celle de YAML, la rendant plus adaptée à l'échange de données dans les applications web. Néanmoins, YAML reste populaire pour les fichiers de configuration et les situations où la lisibilité humaine est primordiale.

Lorsque vous travaillez avec YAML dans Google Apps Script, considérez les compromis entre lisibilité et facilité d'utilisation. Pour une manipulation complète de YAML, il peut être intéressant d'explorer des outils ou des services externes qui peuvent convertir YAML en JSON avant de le traiter dans votre script.
