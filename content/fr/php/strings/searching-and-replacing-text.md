---
date: 2024-01-20 17:58:12.834467-07:00
description: "Chercher et remplacer du texte, c'est un peu comme jouer \xE0 cache-cache\
  \ puis redessiner ce qui \xE9tait cach\xE9. Les programmeurs le font pour mettre\
  \ \xE0 jour des\u2026"
lastmod: '2024-03-13T22:44:57.862631-06:00'
model: gpt-4-1106-preview
summary: "Chercher et remplacer du texte, c'est un peu comme jouer \xE0 cache-cache\
  \ puis redessiner ce qui \xE9tait cach\xE9."
title: Recherche et remplacement de texte
weight: 10
---

## Quoi & Pourquoi ?

Chercher et remplacer du texte, c'est un peu comme jouer à cache-cache puis redessiner ce qui était caché. Les programmeurs le font pour mettre à jour des données, corriger des erreurs, ou changer du contenu dynamiquement.

## Comment faire :

Voici du code. Simple et direct.

```php
<?php
$texte = "Bonjour, monde !";
$texteModifie = str_replace("monde", "univers", $texte);

echo $texteModifie; // Affiche "Bonjour, univers !"
?>
```

Si on veut plus complexe, avec des expressions régulières :

```php
<?php
$texte = "Les chats sont les meilleurs, pas vrai ?";
$texteModifie = preg_replace("/chats|chiens/", "capibaras", $texte);

echo $texteModifie; // Affiche "Les capibaras sont les meilleurs, pas vrai ?"
?>
```

Note : `str_replace` est simple. `preg_replace` est puissant pour motifs complexes.

## Deep Dive

Chercher et remplacer n'est pas né d'hier. Ça remonte aux débuts de l'informatique, quand on avait juste des lignes de commandes. En PHP, `str_replace` et `preg_replace` sont nos outils de prédilection.

Les alternatives ? Bien sûr. On a les fonctions comme `str_ireplace` (pour ignorer la casse) et `substr_replace` (pour une position spécifique). Mais avec `preg_replace`, on a le contrôle avec des expressions régulières, un standard dans la manipulation des textes.

Implémentation ? Côté serveur PHP fait le gros du travail. Il manipule, il remplace, et il renvoie le texte. Avec PHP 8, ça roule encore plus vite et sécurisé.

## Voir Aussi

Pour gratter un peu plus :

- [La documentation officielle de PHP sur str_replace](https://www.php.net/manual/fr/function.str-replace.php)
- [Explication des expressions régulières (Regex)](https://www.php.net/manual/fr/reference.pcre.pattern.syntax.php)
- [Outils en ligne pour tester les Regex](https://regexr.com/)
- [Guide de preg_replace](https://www.php.net/manual/fr/function.preg-replace.php)

C'est tout. Codez bien. Codez propre.
