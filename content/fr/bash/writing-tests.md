---
title:                "Bash: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être un développeur qui se demande pourquoi il est important d'écrire des tests dans votre code Bash. Les tests sont en fait très utiles car ils permettent de s'assurer que votre code fonctionne correctement et qu'il continue de fonctionner correctement même après des modifications ultérieures. Ils peuvent également aider à détecter et à résoudre les erreurs plus rapidement, ce qui vous fait gagner du temps et de l'énergie.

## Comment faire

Pour écrire des tests dans votre code Bash, vous pouvez utiliser le programme de test intégré appelé <code>test</code>. Ce programme permet de vérifier une condition et de renvoyer un code de sortie en conséquence (<code>0</code> si la condition est vraie et <code>1</code> si elle est fausse). Voici un exemple de test simple dans un fichier nommé <code>mon_script.sh</code> :

```Bash
# Define variable
a=10

# Test for variable value
test $a -eq 10

# Check exit status
if [ $? -eq 0 ]; then
  echo "La variable a a une valeur de 10."
else
  echo "La variable a n'a pas une valeur de 10."
fi
```

En exécutant ce script, vous devriez voir la sortie suivante :

```
La variable a a une valeur de 10.
```

Vous pouvez également utiliser le programme de test pour vérifier si des fichiers ou des dossiers existent, si des variables sont définies et bien d'autres choses encore. N'hésitez pas à consulter la documentation pour en savoir plus sur toutes les possibilités offertes par <code>test</code>.

## Approfondissement

En plus de <code>test</code>, il existe d'autres programmes de test tels que <code>[</code> (alias de <code>test</code>) et <code>[[</code>, qui ont des fonctionnalités supplémentaires. De plus, vous pouvez également utiliser des outils externes tels que <code>shunit2</code> pour écrire des tests plus avancés et des outils de génération de rapports comme <code>shellcheck</code> pour améliorer la qualité de votre code Bash.

N'oubliez pas que les tests doivent être bien conçus et exhaustifs pour être vraiment efficaces. Il est également important de les effectuer régulièrement afin de détecter rapidement toute erreur éventuelle.

## Voir aussi

- [Documentation de test Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html#Bash-Conditional-Expressions)
- [Documentation de shunit2](https://github.com/kward/shunit2)
- [Shellcheck](https://www.shellcheck.net/)