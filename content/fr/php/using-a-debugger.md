---
title:                "Utilisation d'un débogueur"
aliases:
- fr/php/using-a-debugger.md
date:                  2024-01-26T03:50:24.783248-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'un débogueur"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/using-a-debugger.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Un débogueur est un outil qui aide les programmeurs à comprendre ce que leur code fait réellement lorsqu'il s'exécute. C'est la loupe qui nous permet de zoomer sur les bugs—ces problèmes agaçants causant l'échec de nos programmes ou produisant des réponses incorrectes—et de les éliminer. Nous utilisons des débogueurs parce qu'ils nous font économiser des heures d'impressions et de conjectures.

## Comment faire :
PHP est fourni avec un débogueur interactif appelé Xdebug. Voici comment l'utiliser.

Tout d'abord, assurez-vous que vous avez Xdebug installé et configuré dans votre fichier `php.ini` :

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

Ensuite, écrivez un simple script PHP contenant un bug :

```PHP
<?php
function add($a, $b) {
    return $a - $b; // Oups ! Cela devrait être un plus, pas un moins
}

$result = add(1, 2);
echo "Le résultat est : $result"; // Le résultat devrait être 3, et non -1
```

En utilisant un IDE comme PhpStorm, positionnez un point d'arrêt en cliquant à côté du numéro de ligne. Lancez le débogueur et observez comment les variables changent au fur et à mesure que vous avancez dans l’exécution. Lorsque vous passez par-dessus la fonction `add`, vous remarquerez que `$result` devient -1, ce qui est inattendu.

## Approfondissement :
Historiquement, PHP était utilisé principalement pour de petits scripts et le débogage consistait à ajouter des instructions `var_dump()` et `print_r()` à travers le code. Avec le temps, PHP devenant un acteur clé dans le développement web, des outils plus sophistiqués comme Xdebug et Zend Debugger ont été adoptés.

Parmi les alternatives à Xdebug, on trouve pcov et phpdbg. Ces outils offrent diverses fonctionnalités mais pourraient ne pas être aussi complets que Xdebug. phpdbg est un débogueur léger, spécifique à PHP, qui est distribué avec PHP depuis la version 5.6, et pcov est un pilote de couverture de code.

Lorsque vous implémentez un débogueur, rappelez-vous que vous ne devriez jamais laisser le débogueur activé sur votre serveur de production, car cela peut exposer des vulnérabilités de sécurité et ralentir les performances.

## Voir aussi :
- [Documentation Xdebug](https://xdebug.org/docs/)
- [Guide de débogage PhpStorm](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net sur phpdbg](https://www.php.net/manual/fr/book.phpdbg.php)
- [pcov sur GitHub](https://github.com/krakjoe/pcov)
