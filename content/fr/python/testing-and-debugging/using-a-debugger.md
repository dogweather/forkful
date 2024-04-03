---
date: 2024-01-26 04:08:55.437362-07:00
description: "D\xE9cortiquons l\u2019utilisation de `pdb`, le d\xE9bogueur int\xE9\
  gr\xE9 de Python. Imaginez un fichier, `buggy.py`, avec un bug sournois : ```Python\
  \ def\u2026"
lastmod: '2024-03-13T22:44:57.241300-06:00'
model: gpt-4-0125-preview
summary: "D\xE9cortiquons l\u2019utilisation de `pdb`, le d\xE9bogueur int\xE9gr\xE9\
  \ de Python."
title: "Utilisation d'un d\xE9bogueur"
weight: 35
---

## Comment faire :
Décortiquons l’utilisation de `pdb`, le débogueur intégré de Python. Imaginez un fichier, `buggy.py`, avec un bug sournois :

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

En exécutant ce script, vous vous attendez à `8`, mais cela provoque juste une erreur de syntaxe. C’est le moment pour le débogueur !

Dans votre terminal, exécutez :
```bash
python -m pdb buggy.py
```

Vous entrerez dans le débogueur, et cela ressemble à ceci :
```Python
> /path_to_file/buggy.py(1)<module>()
-> def add_one(number):
```

Utilisez `l(ist)` pour voir plus de code, `n(ext)` pour passer à la ligne suivante, ou `c(ontinue)` pour continuer à exécuter le script. Quand vous atteignez l'erreur, `pdb` s'arrêtera et vous laissera inspecter.

Après avoir corrigé `number ++ 1` en `number + 1`, redémarrez le débogueur pour tester la correction.
Rappelez-vous, les amis ne laissent pas les amis coder sans filet. C’est dit.

## Plongée profonde
Dans les Âges sombres de la programmation (c.-à-d. avant que les environnements de développement intégrés, ou IDE, ne soient partout), les débogueurs étaient souvent des outils autonomes que vous utilisiez en dehors de votre éditeur de texte. Ils venaient à la rescousse en permettant aux programmeurs d'inspecter l'état de leur logiciel à différents points d'exécution.

En 2023, `pdb` de Python n'est pas le seul en jeu. Les gens pourraient utiliser des IDE comme PyCharm ou Visual Studio Code, qui intègrent leurs propres débogueurs élégants. Ces derniers ajoutent des fonctionnalités pratiques comme les points d'arrêt que vous pouvez définir d’un clic, plutôt que de taper des commandes cryptiques.

Puis il y a `ipdb`, un package installable via pip qui apporte la bonté d'`IPython` au débogage. C’est comme `pdb` sous stéroïdes, avec la complétion par onglet et la coloration syntaxique.

Les débogueurs varient également dans leur mise en œuvre. Certains se rapprochent de l'exécution du programme au niveau de la machine ou du code intermédiaire. D'autres, comme de nombreux débogueurs de langages de haut niveau, exécutent le code dans un environnement spécial qui surveille les états des variables et contrôle le flux d'exécution.

## Voir aussi
Pour tout savoir sur le débogueur de Python, consultez :
- La documentation de `pdb` : https://docs.python.org/3/library/pdb.html

Si vous êtes curieux des alternatives, ces liens vous seront utiles :
- Le dépôt et le guide d'utilisation de `ipdb` : https://github.com/gotcha/ipdb
- Débogage avec Visual Studio Code : https://code.visualstudio.com/docs/python/debugging
- Fonctionnalités de débogage de PyCharm : https://www.jetbrains.com/help/pycharm/debugging-code.html

Bonne chasse aux bugs !
