---
title:                "Bash: Génération de nombres aléatoires"
programming_language: "Bash"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi générer des nombres aléatoires ?

Générer des nombres aléatoires est un élément essentiel de la programmation, et cela peut être utile dans de nombreuses applications. Cela peut être utilisé pour créer des jeux, des simulations, des tests et bien plus encore. Les nombres aléatoires ajoutent de l'imprévisibilité à vos programmes et peuvent rendre votre code plus dynamique et intéressant.

## Comment le faire en Bash ?

En Bash, générer des nombres aléatoires est assez simple. Vous pouvez utiliser la commande `shuf`, qui mélange les lignes d'un fichier et retourne une ligne aléatoire. Par exemple, si vous souhaitez générer un nombre aléatoire compris entre 1 et 100, vous pouvez utiliser la commande suivante :

```Bash
shuf -i 1-100 -n 1
```
Cela retournera un nombre aléatoire entre 1 et 100. Vous pouvez également utiliser la commande `RANDOM`, qui génère un nombre aléatoire compris entre 0 et 32767 à chaque fois qu'elle est exécutée. Voici un exemple de code utilisant `RANDOM` pour générer 5 nombres aléatoires :

```Bash
for i in {1..5}
do
  echo $RANDOM
done
```
Cela produira une sortie semblable à ceci :

```
10878
28057
13947
21219
26056
```

## Plongée dans le sujet

Vous pourriez vous demander comment Bash génère réellement ces nombres aléatoires. La réponse est qu'il utilise le générateur de nombres pseudo-aléatoires de GNU, qui utilise une formule mathématique pour produire des nombres qui semblent aléatoires. Cependant, ces nombres ne sont pas vraiment aléatoires car ils peuvent être reproduits en utilisant la même formule. Cela peut sembler peu fiable, mais pour la plupart des cas d'utilisation courants, cela fonctionne bien.

Si vous souhaitez utiliser des nombres vraiment aléatoires dans votre code Bash, vous pouvez utiliser des sources externes comme un générateur de nombres aléatoires matériel ou des données provenant d'une source en ligne. Cela nécessite des connaissances plus avancées en programmation Bash et n'est pas nécessaire pour la plupart des projets.

## Voir aussi

- [Documentation Bash sur la commande shuf](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)
- [Documentation Bash sur la variable RANDOM] (https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)
- [Article sur les nombres aléatoires en Bash] (https://www.linuxjournal.com/content/bash-tips-random-numbers)