---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:41.088451-07:00
description: "Comment faire : En Clojure, vous pouvez \xE9crire sur stderr en utilisant\
  \ le flux `*err*`. Voici un exemple simple ."
lastmod: '2024-03-13T22:44:57.299390-06:00'
model: gpt-4-0125-preview
summary: "En Clojure, vous pouvez \xE9crire sur stderr en utilisant le flux `*err*`."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

## Comment faire :
En Clojure, vous pouvez écrire sur stderr en utilisant le flux `*err*`. Voici un exemple simple :

```clojure
(.write *err* "Ceci est un message d'erreur.\n")
```

Notez qu'après avoir écrit un message, vous devriez vider le flux pour garantir que le message est immédiatement sorti :

```clojure
(flush)
```

Exemple de sortie sur stderr :
```
Ceci est un message d'erreur.
```

Si vous gérez des exceptions, vous voudrez peut-être imprimer les traces de pile sur stderr. Utilisez `printStackTrace` pour cela :

```clojure
(try
  ;; Code qui pourrait lancer une exception
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

Pour une journalisation d'erreur plus structurée, des bibliothèques tierces comme `timbre` peuvent être configurées pour enregistrer sur stderr. Voici une configuration et utilisation basiques :

D'abord, ajoutez `timbre` à vos dépendances. Puis configurez-le pour utiliser stderr :

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; Désactivation de la journalisation stdout
(timbre/set-config! [:appenders :spit :enabled?] false) ;; Désactivation de la journalisation sur fichier
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; Activation de stderr pour les erreurs

(timbre/error "Une erreur s'est produite lors du traitement de votre demande.")
```

Cela dirigera les messages de niveau d'erreur vers stderr, les rendant distincts de la sortie standard de l'application.
