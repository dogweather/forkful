---
title:    "PHP: Génération de nombres aléatoires"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires peut sembler une tâche peu utile à première vue, mais cela peut en fait être très utile pour certaines applications. Par exemple, les jeux en ligne utilisent souvent des nombres aléatoires pour créer des scénarios uniques, et de nombreux algorithmes utilisent également des nombres aléatoires pour prendre des décisions.

## Comment

Il existe plusieurs façons de générer des nombres aléatoires en PHP, en voici quelques exemples :

```PHP
// Génère un nombre aléatoire compris entre 0 et 100
echo rand(0, 100);

// Génère un nombre aléatoire à virgule flottante compris entre 0 et 1
echo mt_rand() / mt_getrandmax();

// Génère un tableau de 5 nombres aléatoires compris entre 1 et 10
$numbers = array();
for ($i = 0; $i < 5; $i++) {
  $numbers[] = rand(1, 10);
}
print_r($numbers);
```

Ces méthodes utilisent toutes la fonction "rand" ou "mt_rand" qui génère des nombres aléatoires en utilisant l'horloge interne de l'ordinateur comme base.

## Plongée en profondeur

Si vous avez besoin de générer des nombres aléatoires avec des propriétés plus spécifiques, il existe également des fonctions plus avancées en PHP. Par exemple, la fonction "random_int" génère un nombre aléatoire en utilisant une source de chiffres aléatoires plus sûre que le générateur d'horloge interne. De plus, la fonction "shuffle" permet de mélanger un tableau de manière aléatoire.

Il est également important de noter que les nombres aléatoires générés par les ordinateurs sont en fait pseudo-aléatoires, ce qui signifie qu'ils sont déterminés par une formule mathématique plutôt que par un vrai hasard. Cela est dû au fonctionnement des ordinateurs et n'affecte pas leur utilité pour la plupart des applications.

## Voir aussi

- [Documentation officielle de PHP sur les nombres aléatoires](https://www.php.net/manual/fr/function.rand.php)
- [Un article sur les différentes fonctions de génération de nombres aléatoires en PHP](https://www.php.net/manual/fr/function.rand.php)
- [Un tutoriel sur l'utilisation de la fonction "random_int" en PHP](https://www.thoughtco.com/generating-random-integers-with-php-2693679)

Merci d'avoir lu cet article sur la génération de nombres aléatoires en PHP ! En utilisant ces fonctions, vous pourrez facilement intégrer des éléments aléatoires dans vos projets, que ce soit pour des jeux, des algorithmes ou d'autres applications. À vous de jouer maintenant !