---
title:    "Clojure: Création d'un fichier temporaire"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en Clojure?

Lorsqu'on programme en Clojure, il peut être utile de créer un fichier temporaire pour stocker des données temporaires ou pour effectuer des opérations spécifiques. Cela peut également être nécessaire lors de la manipulation de gros volumes de données où il est plus efficace de les stocker temporairement dans un fichier avant de les traiter.

## Comment créer un fichier temporaire en Clojure?

Pour créer un fichier temporaire en Clojure, nous pouvons utiliser la fonction `with-open` qui nous permettra de créer le fichier temporaire et de le fermer automatiquement une fois que nous avons terminé nos opérations. Voici un exemple de code:

```Clojure
(with-open [temp-file (java.io.File/createTempFile "temp" ".txt")]
    (println "Le chemin du fichier temporaire est:" (.getPath temp-file))
    (println "Le nom du fichier temporaire est:" (.getName temp-file)))
```

Output:

```
Le chemin du fichier temporaire est: /var/folders/g9/kq0cr4l14t503xh7b30c8f040000gn/T/temp5170164365431830835.txt
Le nom du fichier temporaire est: temp5170164365431830835.txt
```

Dans cet exemple, nous utilisons la fonction `java.io.File/createTempFile` pour créer notre fichier temporaire en spécifiant un préfixe ("temp") et une extension (".txt"). Nous pouvons ensuite accéder aux différentes propriétés de ce fichier en utilisant les méthodes disponibles dans la classe `java.io.File`.

## Plongée en profondeur dans la création de fichiers temporaires en Clojure

Il est important de noter que la fonction `createTempFile` crée un fichier qui est automatiquement supprimé lorsque le programme s'exécute avec succès. Cependant, si une erreur se produit, le fichier temporaire ne sera pas supprimé automatiquement et devra être supprimé manuellement.

De plus, si nous voulons spécifier un chemin de destination différent pour notre fichier temporaire, nous pouvons utiliser la fonction `createTempFile` avec deux arguments supplémentaires: le chemin et le préfixe du fichier temporaire.

```Clojure
(with-open [temp-file (java.io.File/createTempFile "/mon/chemin/destination/" "temp")]
      (println "Le chemin du fichier temporaire est:" (.getPath temp-file))
      (println "Le nom du fichier temporaire est:" (.getName temp-file)))
```

Output:

```
Le chemin du fichier temporaire est: /mon/chemin/destination/temp9053080553795498028
Le nom du fichier temporaire est: temp9053080553795498028
```

## Voir aussi

- [Documentation officielle Clojure](https://clojure.github.io/api/)
- [Tutoriels Clojure](https://www.tutorialspoint.com/clojure/)
- [Guide du débutant en Clojure](https://www.braveclojure.com/)

Merci d'avoir lu cet article sur la création de fichiers temporaires en Clojure! Nous espérons que cela vous a été utile dans vos projets de programmation. N'hésitez pas à consulter les liens ci-dessus pour plus de ressources sur le langage Clojure. Bonne programmation!