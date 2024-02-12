---
title:                "Gestion des erreurs"
aliases: - /fr/clojure/handling-errors.md
date:                  2024-01-26T00:50:27.438056-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestion des erreurs"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/handling-errors.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
La gestion des erreurs consiste à gérer l'inattendu dans les programmes—comme un videur faisant face à des perturbateurs. Les programmeurs préfèrent que tout se passe en douceur ; la gestion des erreurs permet de maintenir les problèmes en ligne, s'assurant que leur code ne trébuche et ne tombe pas lorsqu'il est confronté à l'inattendu.

## Comment faire :
Clojure, comme ses ancêtres Lisp, s'appuie sur les exceptions pour gérer les erreurs. Voici comment montrer de quoi vous êtes capable lorsque les choses tournent mal.

Lancer une exception est simple :
```Clojure
(throw (Exception. "Oups ! Quelque chose s'est mal passé."))
```

Attraper une exception, vous allez beaucoup le faire :
```Clojure
(try
  ;; code à risque
  (/ 1 0)
  (catch ArithmeticException e
    (println "Impossible de diviser par zéro !"))
  ;; le bloc finally s’exécute quoi qu’il arrive
  (finally 
    (println "Le code de nettoyage se met ici.")))
```
Exemple de sortie pour le bloc catch ci-dessus :
```
Impossible de diviser par zéro !
Le code de nettoyage se met ici.
```

Utiliser `ex-info` et `ex-data` pour un contexte plus riche sur les exceptions :
```Clojure
(try
  ;; provoquant une exception personnalisée
  (throw (ex-info "Erreur personnalisée" {:type :echec-personnalisé}))
  (catch Exception e
    ;; extrayant les données de notre exception personnalisée
    (println (ex-data e))))
```
Exemple de sortie :
```
{:type :echec-personnalisé}
```

## Plongée en Profondeur
L'histoire de la gestion des erreurs en Clojure n'est pas radicalement différente de celle d'autres Lisps ou même de Java (d'où il hérite le mécanisme `try-catch`). C'est pragmatique ; l'utilisation d'exceptions est la voie principale, tout comme Java, mais Clojure offre une saveur fonctionnelle avec `ex-info` et `ex-data` pour des données d'erreur plus riches.

Les alternatives pour la gestion des erreurs en Clojure incluent l'utilisation de constructions monadiques, telles que la monade `either` de bibliothèques comme `cats`, ou core.async pour la propagation d'erreurs basée sur les channels. Cependant, celles-ci sont plus complexes et utilisées dans des scénarios spécifiques.

Historiquement, la gestion des erreurs dans les langages de programmation a évolué des simples retours d'état aux mécanismes de gestion des exceptions plus sophistiqués des langages modernes. Clojure opte pour la simplicité et une touche de programmation fonctionnelle, mélangeant l'ancien et le nouveau.

## Voir Aussi
- Guide de Clojure sur les exceptions : https://clojure.org/guides/exceptions
- Bibliothèque “Cats” pour des approches plus fonctionnelles : https://github.com/funcool/cats
- “Core.async” pour la programmation asynchrone : https://github.com/clojure/core.async
