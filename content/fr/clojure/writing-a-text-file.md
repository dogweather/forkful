---
title:                "Écrire un fichier texte"
html_title:           "Clojure: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire un fichier texte peut sembler simple, mais cela peut être utile pour diverses raisons, telles que la sauvegarde de données, la création de scripts ou la préparation de rapports.

## Comment faire

Pour écrire un fichier texte en utilisant Clojure, il suffit d'utiliser la fonction `spit`. Voici un exemple de code:

```Clojure
(spit "mon_fichier.txt" "Bonjour le monde!")
```

Cela créera un fichier nommé "mon_fichier.txt" contenant la phrase "Bonjour le monde!".

Si vous souhaitez ajouter du contenu à un fichier existant, vous pouvez utiliser la fonction `slurp` pour lire le contenu du fichier, puis utiliser `spit` pour écrire le nouveau contenu. Voici un exemple:

```Clojure
(let [mon_fichier (slurp "mon_fichier.txt")]
  (spit "mon_fichier.txt" (str mon_fichier " Au revoir le monde!")))
```

Cela ajoutera la phrase " Au revoir le monde!" à la fin du fichier.

## Plongée en profondeur

Il existe plusieurs options pour personnaliser l'écriture d'un fichier texte en utilisant Clojure. Par exemple, vous pouvez spécifier le mode d'écriture avec `spit`, tels que `:append` pour ajouter du contenu à la fin du fichier, ou `:truncate` pour écraser le contenu existant.

De plus, vous pouvez également spécifier un encodage spécifique avec le paramètre `:encoding` pour gérer les caractères spéciaux dans votre fichier texte.

## Voir aussi

- [Documentation officielle de Clojure sur la fonction `spit`](https://clojuredocs.org/clojure.core/spit)
- [Un tutoriel sur l'écriture de fichiers texte en Clojure](https://www.baeldung.com/clojure-write-file)