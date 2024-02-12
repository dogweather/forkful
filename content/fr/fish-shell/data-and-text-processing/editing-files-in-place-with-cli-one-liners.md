---
title:                "Modification de fichiers sur place avec des lignes de commande en une seule étape"
aliases: - /fr/fish-shell/editing-files-in-place-with-cli-one-liners.md
date:                  2024-01-27T16:20:44.363202-07:00
model:                 gpt-4-0125-preview
simple_title:         "Modification de fichiers sur place avec des lignes de commande en une seule étape"

tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/editing-files-in-place-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Modifier des fichiers sur place avec des lignes de commande est une pratique qui consiste à apporter des modifications directement aux fichiers depuis la ligne de commande, sans les ouvrir dans un éditeur de texte. Les programmeurs font cela pour gagner du temps et automatiser les tâches d'édition répétitives, rendant leur flux de travail plus fluide et plus efficace.

## Comment faire :

Fish Shell, connu pour ses fonctionnalités conviviales et ses puissantes capacités de script, offre plusieurs manières de modifier des fichiers sur place. Cependant, contrairement à certains autres shells, Fish n'a pas de mécanisme intégré pour la modification sur place (`sed -i` dans Bash, par exemple). Mais ne vous inquiétez pas, vous pouvez tout de même y parvenir avec un peu de créativité et quelques outils externes comme `sed` et `awk`.

### Utiliser `sed` pour des remplacements simples
Pour remplacer toutes les instances de "hello" par "world" dans `file.txt`, vous utiliseriez :
```Fish Shell
sed -i '' 's/hello/world/g' file.txt
```

### Appliquer plusieurs commandes `sed`
Si vous avez besoin d'effectuer plusieurs remplacements, vous pouvez les chaîner ainsi :
```Fish Shell
sed -i '' -e 's/fish/bass/g' -e 's/rainbow/trout/g' file.txt
```

### Utiliser `awk` pour des opérations plus complexes
Pour des opérations trop complexes pour `sed`, `awk` pourrait être l'outil de votre choix. Voici comment doubler le nombre sur chaque ligne :
```Fish Shell
awk '{print $1 * 2}' file.txt > temp && mv temp file.txt
```

### Note sur le traitement des erreurs
Rappelez-vous, lors de l'utilisation de ces outils depuis Fish, capturer les erreurs et comprendre leurs messages est crucial. Utilisez le robuste traitement des erreurs de Fish pour rendre vos scripts plus fiables.

## Plongée profonde

Historiquement, la modification de fichiers sur place a été un pilier de la programmation Unix et Linux, offrant un moyen efficace d'effectuer des modifications rapides sans ouvrir manuellement les fichiers. Des outils comme `sed` et `awk` sont des utilitaires vénérables qui existent depuis les premiers jours d'Unix, devenant indispensables pour les tâches de traitement de texte.

Fish Shell, bien plus moderne et offrant des améliorations en termes d'utilisabilité et de script, manque de modification sur place intégrée principalement en raison de sa philosophie de conception axée sur l'interactivité et la convivialité. L'absence d'une commande de modification sur place native dans Fish souligne l'importance des outils externes dans les écosystèmes de type Unix.

Les alternatives pour la modification sur place dans Fish incluent l'utilisation de fichiers temporaires ou l'exploitation de lignes de commande Perl ou Python, qui peuvent offrir plus de flexibilité ou de lisibilité pour les tâches complexes.

Par exemple, en utilisant Perl :
```Fish Shell
perl -pi -e 's/find/replace/g' file.txt
```
Ou Python :
```Fish Shell
python -c "import re, sys; [sys.stdout.write(re.sub('pattern', 'replacement', line)) for line in sys.stdin]" < file.txt > temp && mv temp file.txt
```

En termes de mise en œuvre, lorsque vous effectuez une modification sur place, sous le capot, ces outils créent généralement un fichier temporaire, y écrivent les modifications, puis remplacent le fichier original par la version modifiée. Cette approche garantit que le processus de modification du fichier ne corrompt pas ou ne perd pas de données si une erreur se produit pendant l'opération.

Comprendre ces outils et méthodes permet aux programmeurs Fish Shell d'incorporer efficacement la modification sur place dans leurs scripts, comblant le fossé entre les fonctionnalités conviviales de Fish et la puissance brute des utilitaires de traitement de texte Unix traditionnels.
