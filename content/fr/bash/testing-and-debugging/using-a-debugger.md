---
date: 2024-01-26 03:47:31.296164-07:00
description: "Comment faire : Bash n'est pas fourni avec un d\xE9bogueur int\xE9gr\xE9\
  \ comme certains autres langages, mais vous pouvez utiliser des commandes int\xE9\
  gr\xE9es comme\u2026"
lastmod: '2024-03-13T22:44:58.001354-06:00'
model: gpt-4-0125-preview
summary: "Bash n'est pas fourni avec un d\xE9bogueur int\xE9gr\xE9 comme certains\
  \ autres langages, mais vous pouvez utiliser des commandes int\xE9gr\xE9es comme\
  \ `set -x` pour tracer ce qui se passe."
title: "Utilisation d'un d\xE9bogueur"
weight: 35
---

## Comment faire :
Bash n'est pas fourni avec un débogueur intégré comme certains autres langages, mais vous pouvez utiliser des commandes intégrées comme `set -x` pour tracer ce qui se passe. Ou, pour une amélioration, il y a `bashdb`, un véritable débogueur pour parcourir votre code pas à pas. Voici un aperçu :

```Bash
# Utiliser set -x pour déboguer
set -x
echo "Commencer le débogage"
my_var="Bonjour, monde du débogage !"
echo $my_var
set +x

# Utiliser bashdb
# Installez bashdb avec votre gestionnaire de paquets, par ex., apt, yum, brew.
# Pour déboguer un script appelé my_script.sh :
bashdb my_script.sh
```

Sortie lors de l'exécution avec `set -x`:
```Bash
+ echo 'Commencer le débogage'
Commencer le débogage
+ my_var='Bonjour, monde du débogage !'
+ echo 'Bonjour, monde du débogage !'
Bonjour, monde du débogage !
+ set +x
```

## Approfondissement
Historiquement, le débogage des scripts Bash signifiait éparpiller votre code avec des instructions `echo`. Mais ensuite est venu `set -x`, nous donnant un aperçu de l'exécution en temps réel sans impressions manuelles. Et pour ceux qui désirent plus de contrôle, le débogueur `bashdb` est apparu, inspiré par le débogueur gdb pour C/C++.

Quant aux alternatives, au-delà des commandes `set` (`-x`, `-v`, `-e`), d'autres options incluent la redirection de sortie vers un fichier pour analyse ou l'utilisation d'outils externes comme ShellCheck pour une analyse statique.

En termes de mise en œuvre, `set -x` est facile ; c'est une option native de Bash qui imprime les commandes et leurs arguments au fur et à mesure de leur exécution. `bashdb`, d'autre part, permet de parcourir le code pas à pas, de définir des points d'arrêt et d'évaluer des expressions - des choses qui vous donnent une chance de lutter contre des bugs plus insaisissables.

## Voir Aussi
- Projet Bash Debugger : http://bashdb.sourceforge.net/
- "Pro Bash Programming" par Chris Johnson et Jayant Varma pour des scripts avancés.
- ShellCheck pour l'analyse statique : https://www.shellcheck.net/
