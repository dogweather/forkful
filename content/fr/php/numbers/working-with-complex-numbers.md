---
title:                "Manipulation des nombres complexes"
aliases:
- /fr/php/working-with-complex-numbers.md
date:                  2024-01-26T04:43:48.006327-07:00
model:                 gpt-4-0125-preview
simple_title:         "Manipulation des nombres complexes"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les nombres complexes ont une partie réelle et une partie imaginaire, généralement écrits sous la forme `a + bi`. Ils sont cruciaux dans les mathématiques avancées, la physique, l'ingénierie et certains algorithmes informatiques. Les programmeurs travaillent avec eux pour gérer les calculs impliquant des racines carrées de nombres négatifs et des fonctions oscillantes.

## Comment:
PHP fournit un support intégré pour les nombres complexes en utilisant l'extension `ext-intl` avec la classe `NumberFormatter`. Voici un exemple :

```php
// Assurez-vous que l'extension intl est chargée
if (!extension_loaded('intl')) {
    die("L'extension intl n'est pas activée. Veuillez l'activer pour exécuter ce code.");
}

function addComplexNumbers($a, $b) {
    // Utilise NumberFormatter pour analyser et formater les nombres complexes
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // Analyse les nombres complexes à partir de chaînes
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // Effectue l'addition
    $som = $numA + $numB;

    // Formate le résultat comme un nombre complexe
    return $formatter->format($som);
}

echo addComplexNumbers('5+3i', '2+7i'); // Sortie : 7+10i
```

## Plongée Profonde
Avant `ext-intl`, PHP n'avait pas de support natif pour les nombres complexes. Les développeurs utilisaient des fonctions ou des bibliothèques de classes personnalisées pour gérer les nombres complexes. Les opérations complexes pouvaient être fastidieuses et sujettes à erreur, mais `ext-intl` fournit une manière internationalisée de présenter et d'analyser les nombres complexes alignée avec la bibliothèque ICU.

Cependant, pour les opérations mathématiques lourdes, certains pourraient utiliser des bibliothèques externes écrites dans des langages plus adaptés aux mathématiques (comme C ou Python) et interfacer avec elles via PHP. En ce qui concerne l'implémentation, `ext-intl` s'en occupe en arrière-plan, assurant une arithmétique précise tout en abstrayant la complexité pour le développeur.

Historiquement, les nombres complexes étaient mal vus étant qualifiés d'"imaginaires", mais ils sont depuis devenus fondamentaux dans divers champs scientifiques et mathématiques, révélant plus sur leur importance réelle que leur statut imaginaire n'a jamais suggéré.

## Voir Aussi
- [Manuel PHP sur NumberFormatter](https://www.php.net/manual/en/class.numberformatter.php)
- [Wikipédia sur les nombres complexes](https://en.wikipedia.org/wiki/Complex_number)
- [PHP : La Bonne Manière - Travailler avec les Types de Données](https://phptherightway.com/#data_types)
