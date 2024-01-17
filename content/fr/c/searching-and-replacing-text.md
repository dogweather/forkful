---
title:                "Recherche et remplacement de texte"
html_title:           "C: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

La recherche et le remplacement de texte font partie des tâches courantes des programmeurs. Cela consiste à trouver une chaîne de caractères spécifique dans un ensemble de données, et à la remplacer par une autre chaîne. Les programmeurs utilisent cette technique pour corriger des erreurs, mettre à jour du code obsolète ou pour effectuer des modifications massives dans leur code.

## How to:

Pour effectuer une recherche et un remplacement de texte en C, vous pouvez utiliser la fonction `str_replace` de la bibliothèque `<string.h>`. Voici un exemple de code :

```
#include <stdio.h>
#include <string.h>

int main() {
	char str[] = "Bonjour le monde !";
	char old_str[] = "le monde";
	char new_str[] = "mon ami";

	printf("Avant la recherche et le remplacement : %s\n", str); // Avant la recherche et le remplacement : Bonjour le monde !

	str_replace(str, old_str, new_str);

	printf("Après la recherche et le remplacement : %s\n", str); // Après la recherche et le remplacement : Bonjour mon ami !

	return 0;
}
```

Vous pouvez également l'utiliser pour remplacer toutes les occurrences d'une chaîne de caractères :

```
#include <stdio.h>
#include <string.h>

int main() {
	char str[] = "Voici une phrase avec des chiffres : 123456789";
	char old_str[] = "123";
	char new_str[] = "ABC";

	printf("Avant la recherche et le remplacement : %s\n", str); // Avant la recherche et le remplacement : Voici une phrase avec des chiffres : 123456789

	str_replace(str, old_str, new_str);

	printf("Après la recherche et le remplacement : %s\n", str); // Après la recherche et le remplacement : Voici une phrase avec des chiffres : ABC456789

	return 0;
}
```

## Deep Dive

La fonction `str_replace` a été introduite en C99. Avant cela, les programmeurs devaient réaliser eux-mêmes une fonction de recherche et de remplacement en utilisant des boucles et des pointeurs. Bien qu'il existe d'autres fonctions dans la bibliothèque `<string.h>` pour réaliser des opérations similaires, `str_replace` offre une solution plus rapide et efficace pour les chaînes de caractères.

Il existe également des alternatives à l'utilisation de la fonction `str_replace`, comme l'utilisation de la bibliothèque `regex.h` pour effectuer des recherches et des remplacements à l'aide d'expressions régulières. Cependant, cela peut être plus complexe pour les débutants.

En termes d'implémentation, la fonction `str_replace` utilise un algorithme basé sur la méthode de Boyer-Moore pour trouver et remplacer les chaînes de caractères. Cet algorithme est plus efficace que les méthodes basées sur des boucles et des pointeurs, car il utilise un tableau de sauts précalculé pour effectuer la recherche.

## See Also

- [La bibliothèque `<string.h>` en C (en anglais)](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [La bibliothèque `regex.h` en C (en anglais)](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)