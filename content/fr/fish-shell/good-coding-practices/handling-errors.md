---
date: 2024-01-26 00:52:14.006850-07:00
description: "La gestion des erreurs permet \xE0 votre script de faire face de mani\xE8\
  re \xE9l\xE9gante \xE0 l'inattendu. Nous le faisons pour g\xE9rer les \xE9checs\
  \ sans donner des\u2026"
lastmod: '2024-02-25T18:49:54.959024-07:00'
model: gpt-4-1106-preview
summary: "La gestion des erreurs permet \xE0 votre script de faire face de mani\xE8\
  re \xE9l\xE9gante \xE0 l'inattendu. Nous le faisons pour g\xE9rer les \xE9checs\
  \ sans donner des\u2026"
title: Gestion des erreurs
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La gestion des erreurs permet à votre script de faire face de manière élégante à l'inattendu. Nous le faisons pour gérer les échecs sans donner des cheveux gris à nos utilisateurs.

## Comment faire :
Pour capturer les erreurs dans Fish, appuyez-vous sur la commande `status` et les conditionnels. Disons que la commande `ping` échoue ; voici comment détecter cela :

```fish
ping -c 1 example.com
if not status is-success
    echo "Quelque chose d'étrange s'est produit avec le ping."
end
```

Sortie exemple si `ping` échoue :

```
Quelque chose d'étrange s'est produit avec le ping.
```

Pour gérer un code d'erreur spécifique, utilisez `status --is` :

```fish
false
if status --is 1
    echo "Erreur capturée avec le code 1."
end
```

Sortie exemple :
```
Erreur capturée avec le code 1.
```

Pour une approche plus robuste, envisagez l'utilisation d'une fonction :

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping échoué avec le statut $status"
        return 1
    end
end

try_ping
```

## Plongée Profonde
La gestion des erreurs dans Fish ne correspond pas au paradigme `try/catch` que vous pourriez connaître des langages de plus haut niveau. Au lieu de cela, vous disposez de statuts de sortie simples fournis par la commande `status`.

Historiquement, dans les systèmes de type Unix, un statut de sortie de `0` signifie le succès, tandis que toute valeur non nulle indique une erreur, ce qui reflète communément différentes raisons d'échec. Cette convention est employée par la plupart des utilitaires en ligne de commande et donc, par Fish lui-même.

Les alternatives aux vérifications de `status` dans Fish incluent la gestion des signaux via `trap` dans d'autres shells, mais Fish préfère des vérifications de statut plus explicites, car c'est plus propre et moins sujet à des effets secondaires.

Du point de vue de la mise en œuvre, la gestion des erreurs dans Fish reste simple mais puissante, en grande partie grâce à sa nature non bloquante et à l'accent mis sur une syntaxe claire, comme le montrent les exemples. Les codes d'erreurs s'intègrent bien avec les fonctions, permettant une gestion des erreurs modulaire et lisible.

## Voir Aussi
- Documentation Fish sur les conditionnels : https://fishshell.com/docs/current/language.html#conditionals
- Tutoriel Fish sur la gestion des erreurs : https://fishshell.com/docs/current/tutorial.html#error-handling
