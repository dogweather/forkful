---
title:                "Fish Shell: Telechargement d'une page web"
simple_title:         "Telechargement d'une page web"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Pourquoi

Il y a plusieurs raisons pour lesquelles vous pourriez vouloir télécharger une page web à l'aide de Fish Shell. Peut-être que vous voulez sauvegarder une page pour la lire hors ligne, ou extraire des données d'une page pour les utiliser dans un script. Quelle que soit la raison, Fish Shell propose des outils pratiques pour faciliter le téléchargement de pages web.

# Comment faire

Télécharger une page web à l'aide de Fish Shell est assez simple. Tout d'abord, nous avons besoin d'installer un gestionnaire de paquets pour Fish Shell appelé Oh My Fish. Pour cela, ouvrez votre terminal et entrez la commande suivante :

```Fish Shell
curl -L https://get.oh-my.fish | fish
```

Une fois que Oh My Fish est installé, vous pouvez utiliser sa fonctionnalité intégrée appelée "wget" pour télécharger une page web. Par exemple, si vous souhaitez télécharger la page d'accueil de Fish Shell, vous pouvez utiliser cette commande :

```Fish Shell
wget https://fishshell.com/ -O fish-shell.html
```

Cette commande téléchargera la page et la sauvegardera dans un fichier HTML appelé "fish-shell.html". Vous pouvez également spécifier le chemin vers lequel vous souhaitez enregistrer le fichier, en remplaçant "fish-shell.html" par le chemin désiré.

# Plongeon en profondeur

Il y a beaucoup plus de choses que vous pouvez faire avec la fonction "wget" de Oh My Fish. Par exemple, vous pouvez utiliser des options différentes pour personnaliser votre téléchargement. La commande suivante, par exemple, vous permet de spécifier un user-agent pour le téléchargement :

```Fish Shell
wget -U "Fish Shell User" https://fishshell.com/ -O fish-shell.html
```

Cela peut être utile si vous souhaitez télécharger une page qui n'est pas accessible sans un certain user-agent spécifique.

Il existe également d'autres outils de Fish Shell qui peuvent être utilisés pour télécharger des pages web, tels que cURL et HTTPie. N'hésitez pas à explorer ces options pour trouver la plus adaptée à vos besoins.

# Voir aussi

- [Documentation Fish Shell sur wget](https://fishshell.com/docs/current/cmds/wget.html)
- [Oh My Fish - Gestionnaire de paquets pour Fish Shell](https://github.com/oh-my-fish/oh-my-fish)
- [Apprendre les bases de Fish Shell](https://fishshell.com/docs/current/tutorial.html)