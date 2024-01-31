---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Écrire des tests c'est vérifier que ton code fait ce qu'il doit faire. Les programmeurs testent pour éviter les bugs et s'assurer que tout tourne bien après des modifications.

## How to: (Comment faire : )
Pour tester, on utilise des commandes 'assert'. Voici un exemple simple:

```Fish Shell
function test_my_function
    set result (my_function arg1 arg2)
    assert -- "$result" = "expected_output" "Test failed: my_function did not return the expected output"
end

test_my_function
```

Si `my_function` a fonctionné, tu ne verras rien. Si ça foire, tu verras le message de l'assert.

## Deep Dive (Plongeon Profond)
Historiquement, les tests dans les scripts shell étaient primitifs, mais nécessaires. Comparé à des frameworks de tests modernes dans les langages comme Python (pytest) ou JavaScript (Jest), les outils de test pour le shell étaient limités. En Fish, tu peux utiliser `assert` ou te construire un petit framework qui correspond à tes besoins. L'important, c'est de tester les entrées et sorties de tes fonctions pour te couvrir en cas de pépins.

## See Also (Voir Aussi)
- [Fish Documentation](https://fishshell.com/docs/current/index.html)
- [Awesome Fish](https://github.com/jorgebucaran/awesome-fish): une liste de frameworks et outils pour Fish
- [fisher](https://github.com/jorgebucaran/fisher): un package manager pour installer des plugins de tests pour Fish.
