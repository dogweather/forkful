---
title:                "Recherche et remplacement de texte"
date:                  2024-02-01T22:01:09.209367-07:00
model:                 gpt-4-0125-preview
simple_title:         "Recherche et remplacement de texte"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/google-apps-script/searching-and-replacing-text.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La recherche et le remplacement de texte dans Google Apps Script impliquent d'identifier de manière programmatique des chaînes spécifiques dans un document, un tableur ou tout autre type de contenu Google Apps, et de les substituer par d'autres valeurs textuelles. Les programmeurs utilisent cette fonctionnalité pour automatiser l'édition de grands volumes de contenu, corriger des erreurs communes, standardiser la terminologie à travers des documents, ou insérer des données dynamiques dans des modèles.

## Comment faire :

Google Apps Script offre une manière directe de rechercher et remplacer du texte, en particulier au sein de Google Docs et Sheets. Voici des exemples pour les deux.

### Google Docs :

Pour rechercher et remplacer du texte dans un document Google, vous interagirez principalement avec la classe `DocumentApp`.

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // Pour rechercher et remplacer une phrase spécifique
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// Usage
searchReplaceInDoc();
```

Ce fragment de code recherche toutes les occurrences de `'searchText'` dans le document Google actif et les remplace par `'replacementText'`.

### Google Sheets :

De manière similaire, dans Google Sheets, vous pouvez utiliser `SpreadsheetApp` pour effectuer des opérations de recherche et de remplacement :

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // Rechercher et remplacer dans la feuille active actuellement
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// Usage
searchReplaceInSheet();
```

Dans cet exemple, `createTextFinder('searchText')` recherche dans la feuille active 'searchText', et `replaceAllWith('replacementText')` remplace toutes les occurrences par 'replacementText'.

## Exploration Approfondie

La fonctionnalité de recherche et de remplacement dans Google Apps Script est fortement influencée par sa nature basée sur le web, permettant aux scripts de manipuler du texte à travers diverses applications Google de manière transparente. Historiquement, cette capacité s'inscrit dans le contexte plus large du traitement et de la manipulation de texte en programmation, où les expressions régulières et les fonctions de chaîne dans des langues telles que Perl et Python ont établi des normes élevées de flexibilité et de puissance.

Bien que la fonctionnalité de recherche et de remplacement de Google Apps Script soit puissante pour des substitutions simples, elle manque des capacités complètes des expressions régulières trouvées dans certaines autres langues. Par exemple, bien que vous puissiez utiliser des expressions régulières basiques dans `createTextFinder` dans Google Sheets, les options pour des motifs complexes et la manipulation sont limitées comparées à Perl ou Python.

Pour des besoins de traitement de texte plus avancés, les programmeurs pourraient avoir recours à exporter le contenu de Google Docs ou Sheets vers un format qui peut être traité à l'extérieur avec des langues plus puissantes ou employer Google Apps Script pour appeler des API ou services externes qui offrent des capacités de manipulation de texte plus sophistiquées.

Malgré ces limitations, pour la plupart des tâches de recherche et de remplacement typiques au sein de l'écosystème des applications Google, Google Apps Script offre une solution simple, efficace et hautement intégrable, adaptée aux besoins d'automatisation et de scriptage au sein de la suite d'outils de productivité de Google.
