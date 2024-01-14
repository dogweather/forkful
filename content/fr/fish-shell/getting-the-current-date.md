---
title:    "Fish Shell: Obtenir la date actuelle"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi

La programmation peut sembler intimidante pour certains, mais en utilisant un langage de script convivial tel que Fish Shell, vous pouvez accomplir des tâches intéressantes sans trop de difficulté. Aujourd'hui, nous allons parler d'une tâche simple mais très utile pour tout utilisateur de Fish Shell : obtenir la date actuelle.

## Comment Faire

La bonne nouvelle est que Fish Shell a une fonction intégrée pour obtenir la date actuelle. Tout ce que vous avez à faire est de taper la commande suivante dans votre terminal :

```Fish Shell
date +%d/%m/%Y
```

Cela vous donnera en sortie la date actuelle au format JJ/MM/AAAA. Bien sûr, vous pouvez modifier le format selon vos préférences en utilisant les symboles spéciaux disponibles pour la commande `date`. Par exemple, si vous voulez la date au format AAAA-MM-JJ, vous pouvez utiliser `date +%Y-%m-%d`.

Vous pouvez également utiliser la commande `man date` pour obtenir plus d'informations sur les options disponibles pour cette commande et ainsi personnaliser votre sortie selon vos besoins.

## Plongée en Profondeur

Maintenant, vous vous demandez peut-être comment cela fonctionne en coulisses. Eh bien, Fish Shell utilise en fait une version améliorée de la commande `date` disponible dans la plupart des systèmes Unix. En utilisant le `%` suivi de lettres spéciales, la commande `date` sera en mesure de formater la date selon vos spécifications. Vous pouvez trouver la liste complète des symboles disponibles dans la [documentation officielle de Fish Shell](https://fishshell.com/docs/current/commands.html#date).

De plus, sachez que la fonction `date` utilise la variable système `$LANG` pour déterminer le format de la date. Si vous voulez utiliser un format différent, vous pouvez simplement définir cette variable à votre format désiré avant d'exécuter la commande.

## Voir Aussi

- [La documentation officielle de Fish Shell sur la commande `date`](https://fishshell.com/docs/current/commands.html#date)
- [La documentation officielle de la commande `date` dans les systèmes Unix](https://www.unix.com/man-page/linux/1/date/)
- [Un guide détaillé sur la commande `date` de Linuxize](https://linuxize.com/post/linux-date-command/)