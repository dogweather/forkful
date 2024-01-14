---
title:    "Bash: Capitaliser une chaîne de caractères"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles vous pourriez vouloir capitaliser une chaîne de caractères en Bash. Par exemple, cela pourrait être nécessaire pour respecter des conventions de nommage dans votre code ou pour formater des données avant de les utiliser dans une autre partie de votre programme.

## Comment faire

La méthode la plus simple pour capitaliser une chaîne en Bash est d'utiliser la commande `tr`. Voici un exemple de code avec une entrée et une sortie d'écran :

```bash
# Définir la chaîne d'entrée
input_string="bonjour tout le monde"

# Utiliser tr pour capitaliser la chaîne
capitalized_string=$(echo "$input_string" | tr a-z A-Z)

# Sortie : BONJOUR TOUT LE MONDE
echo $capitalized_string
```

Vous pouvez également utiliser la commande `sed` pour capitaliser la première lettre d'une chaîne. Voici un autre exemple de code avec une entrée et une sortie d'écran :

```bash
# Définir la chaîne d'entrée
input_string="hello world"

# Utiliser sed pour capitaliser la première lettre
capitalized_string=$(echo "$input_string" | sed -e "s/^./\U&/")

# Sortie : Hello world
echo $capitalized_string
```

## Plongée profonde 

Si vous souhaitez capitaliser une chaîne selon des règles spécifiques, vous pouvez utiliser des commandes telles que `cut` ou `awk` pour diviser la chaîne en plusieurs parties, puis les capitaliser individuellement.

Par exemple, si vous voulez capitaliser uniquement les trois premières lettres d'une chaîne, vous pouvez utiliser le code suivant :

```bash
# Définir la chaîne d'entrée
input_string="bonjour tout le monde"

# Utiliser cut pour diviser la chaîne en trois parties
first_letters=$(echo "$input_string" | cut -c1-3)

# Utiliser tr pour capitaliser les trois premières lettres
capitalized_string=$(echo "$first_letters" | tr a-z A-Z)

# Concaténer les parties de la chaîne
capitalized_string="$capitalized_string"$(echo "$input_string" | cut -c4-)

# Sortie : BONjour tout le monde
echo $capitalized_string
```

## Voir également

- [Guide Bash pour débutants](https://www.tecmint.com/learn-bash-rename-files/)
- [Documentation officielle sur tr](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html#tr-invocation)
- [Documentation officielle sur sed](https://www.gnu.org/software/sed/manual/sed.html)