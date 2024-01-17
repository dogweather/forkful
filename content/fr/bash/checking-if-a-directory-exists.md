---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Bash: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Quoi et pourquoi?
Vérifier si un répertoire existe est une tâche courante pour les programmeurs en Bash. Cela leur permet de s'assurer qu'un répertoire nécessaire pour leur script est bien présent, avant de poursuivre l'exécution du programme.

# Comment faire:
Il existe plusieurs façons de vérifier si un répertoire existe en Bash. Voici deux exemples :
```Bash
# Vérifier si un répertoire existe et afficher un message en conséquence
if [ -d "répertoire" ]; then
  echo "Le répertoire existe"
else
  echo "Le répertoire n'existe pas"
fi
```

```Bash
# Vérifier si un répertoire existe et créer le répertoire s'il n'existe pas
if [ ! -d "nouveau_répertoire" ]; then
  mkdir "nouveau_répertoire"
  echo "Le répertoire a été créé"
fi
```

# Plongée en profondeur:
La vérification de l'existence d'un répertoire en Bash remonte aux débuts du langage, dans les années 80. Il existe également une commande dédiée spécialement pour cette tâche, "test -d", mais elle est moins couramment utilisée que la syntaxe "[ -d ]" présentée ci-dessus.

Des alternatives existent également, comme l'utilisation de la commande "ls" et la vérification de la sortie pour déterminer si un répertoire est présent. Cependant, cette méthode peut être moins fiable car la sortie de "ls" peut varier en fonction de la configuration de l'environnement.

# Voir aussi:
Pour en savoir plus sur l'utilisation de Bash, consultez la documentation officielle en ligne : https://www.gnu.org/software/bash/ ou des sites tels que https://www.shellscript.sh/. Et bien sûr, n'hésitez pas à fouiller le web pour trouver des exemples d'utilisation de la vérification de l'existence d'un répertoire en Bash.