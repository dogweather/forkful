---
title:    "Clojure: Utiliser des expressions régulières"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil puissant pour rechercher et manipuler des motifs de texte. Elles peuvent être utiles pour filtrer des données, valider des saisies utilisateur et bien d'autres tâches liées à la manipulation de chaînes de caractères.

## Comment faire

Les expressions régulières sont disponibles en tant que bibliothèque standard dans Clojure, donc pas besoin d'installer quoi que ce soit de supplémentaire. Voici un exemple simple pour rechercher une séquence de chiffres dans une chaîne de caractères :

```Clojure
(re-find #"[0-9]+" "Il y a 23 chats dans le jardin.")
```

Cela renverra "23" comme résultat, car c'est la première séquence de chiffres trouvée dans la chaîne de caractères. Voici quelques autres opérations courantes avec les expressions régulières :

- Rechercher un motif précis : ```Clojure (re-find #"chat" "Il y a 23 chats dans le jardin.") ```
- Remplacer un motif par un autre : ```Clojure (re-seq #"chat" "Il y a 23 chats dans le jardin.") ```
- Valider une adresse email : ```Clojure
(re-matches #"[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}" "example@email.com")
```

Il existe également des fonctions plus avancées pour la manipulation de chaînes de caractères avec des expressions régulières, telles que re-find-all et re-groups. Pour plus d'informations sur l'utilisation des expressions régulières en Clojure, consultez la documentation officielle.

## Plongée en profondeur

Les expressions régulières peuvent sembler intimidantes au début, avec leurs motifs complexes et cryptiques. Mais une fois que vous en aurez compris les bases, elles deviendront un outil précieux dans votre arsenal de programmation.

Voici quelques conseils pour vous aider à maîtriser les expressions régulières en Clojure :

- Utilisez des sites comme Regex101 pour tester vos expressions régulières en temps réel et comprendre comment elles fonctionnent.
- Utilisez des quantificateurs tels que + et * pour rechercher des motifs plus flexibles dans vos chaînes de caractères.
- Explorez les opérations avancées telles que la capture de groupes et la rétro-références pour des fonctions de manipulation de chaînes plus puissantes.

## Voir aussi

- [Documentation officielle Clojure sur les expressions régulières](https://clojure.org/reference/regular_expressions)
- [Exemples d'utilisation des expressions régulières avec Clojure](https://blog.mycator.com/2017/09/using-regular-expressions-in-clojure.html)
- [Tutoriel vidéo sur les expressions régulières en Clojure](https://www.youtube.com/watch?v=F3g5EE5Goxc)