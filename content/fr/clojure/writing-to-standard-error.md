---
title:    "Clojure: Écrire sur l'erreur standard"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

Écrire vers l'erreur standard est un moyen pratique et utile pour afficher des informations importantes sur les erreurs et les problèmes dans votre code Clojure. Cela peut vous aider à déboguer plus efficacement et à améliorer la qualité de votre code.

## Comment faire

Pour écrire vers l'erreur standard en Clojure, vous pouvez utiliser la fonction ```prn``` comme ceci :
```Clojure
(prn "Message d'erreur")
```
Cela affichera le message "Message d'erreur" vers l'erreur standard.

Vous pouvez également utiliser la fonction ```println``` pour afficher le message sans les guillemets :
```Clojure
(println "Message d'erreur")
```

Dans les deux cas, le message sera affiché dans la console ou le terminal que vous utilisez.

## Approfondissement

Lorsque vous écrivez vers l'erreur standard, vous pouvez également utiliser des arguments supplémentaires pour afficher des informations supplémentaires sur l'erreur. Par exemple, dans un bloc try/catch, vous pouvez utiliser la fonction ```ex-info``` pour inclure des informations sur l'exception dans le message d'erreur :
```Clojure
(try
  (do-something)
  (catch Exception e
    (prn (ex-info "Une erreur s'est produite" {:cause e :trace (.getStackTrace e)}))))
``` 
Cela affichera le message "Une erreur s'est produite" ainsi que des informations sur l'exception et sa trace dans l'erreur standard.

Vous pouvez également utiliser la fonction ```with-bindings``` pour afficher les valeurs des variables locales dans votre message d'erreur. Par exemple :
```Clojure
(with-bindings {a 5 b 10}
  (prn "a vaut" a "et b vaut" b))
```
Cela affichera le message "a vaut 5 et b vaut 10" dans l'erreur standard.

## Voir aussi

- [La documentation officielle de Clojure](https://clojure.org/)
- [Un guide pour déboguer en Clojure](https://purelyfunctional.tv/guide/clojure-debugging/)
- [Un tutoriel de Clojure pour les débutants](https://clojure.org/guides/getting_started)