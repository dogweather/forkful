---
title:                "Retirer les guillemets d'une chaîne"
aliases: - /fr/clojure/removing-quotes-from-a-string.md
date:                  2024-01-26T03:39:45.071975-07:00
model:                 gpt-4-0125-preview
simple_title:         "Retirer les guillemets d'une chaîne"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Supprimer les guillemets d'une chaîne signifie se débarrasser de ces ennuyeux caractères de guillemets doubles ou simples qui encapsulent votre texte. Les programmeurs font cela pour nettoyer les données, assurer l'uniformité, ou préparer les chaînes pour le traitement où les guillemets sont indésirables ou peuvent causer des erreurs.

## Comment faire :
Dans Clojure, les chaînes sont immuables, donc quand nous parlons de "supprimer les guillemets", nous parlons réellement de créer une nouvelle chaîne sans guillemets. Voici l'essentiel en utilisant `clojure.string/replace` :

```clojure
(require '[clojure.string :as str])

; Ditchons ces guillemets doubles
(defn remove-double-quotes [s]
  (str/replace s #"\"" ""))

; Et éjectons les guillemets simples
(defn remove-single-quotes [s]
  (str/replace s #"\'" ""))

; Exemple d'utilisation :
(remove-double-quotes "\"Bonjour, le Monde !\"") ; => "Bonjour, le Monde !"
(remove-single-quotes "'Bonjour, le Monde !'")   ; => "Bonjour, le Monde !"
```
Vous voulez gérer les guillemets simples et doubles en un seul coup ? Jetez un œil à ceci :

```clojure
(defn remove-quotes [s]
  (str/replace s #"[\"\']" ""))

; Exemple d'utilisation :
(remove-quotes "\"Bonjour, 'Clojure' Monde !\"") ; => "Bonjour, Clojure Monde !"
```

## Exploration Approfondie
À l'époque où les données étaient plus désordonnées qu'une chambre d'enfant, les guillemets dans les chaînes étaient la norme pour désigner le texte. Mais à mesure que l'informatique a évolué, les guillemets sont devenus plus que de simples délimiteurs de texte—ils ont pris un rôle syntaxique dans les langages de programmation.

Clojure, avec son héritage Lisp, n'utilise pas les guillemets de la même manière que certains autres langages pourraient le faire. Ils sont utilisés pour dénoter les chaînes, c'est sûr, mais ils ont également un rôle spécial dans la création de littéraux. Néanmoins, supprimer les guillemets des chaînes reste une tâche intemporelle.

Pourquoi ne pas simplement trancher les extrémités d'une chaîne ? Eh bien, cela suppose que vos guillemets sont toujours collés au début et à la fin de votre chaîne comme une paire de grands-parents trop affectueux. Les données du monde réel sont plus désordonnées. Entrez les expressions régulières (regex), qui vous permettent de cibler ces guillemets peu importe où ils se cachent.

Des alternatives ? Bien sûr, vous pouvez vous la jouer sophistiqué avec `subs`, `trim`, `triml`, `trimr`, ou même des transducteurs si vous voulez frimer. Mais `replace` avec regex, c'est comme apporter un sabre laser à un combat au couteau—ça coupe court à la discussion.

## Voir Aussi
Si votre cerveau démange pour plus de bonnes astuces de manipulation de chaînes en Clojure, ces miettes de pain pourraient aider :

- ClojureDocs sur `clojure.string/replace` : https://clojuredocs.org/clojure.string/replace
- Les expressions régulières dans Clojure : https://clojure.org/guides/learn/syntax#_regex
- L'interopérabilité Java pour la manipulation des chaînes (Clojure s'exécute sur la JVM après tout) : https://clojure.org/reference/java_interop#_working_with_strings

Ne vous arrêtez pas simplement à la suppression des guillemets. Il y a tout un monde de magie des chaînes là-bas dans le pays de Clojure qui attend d'être découvert.
