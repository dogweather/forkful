---
title:    "Gleam: Écrire un fichier texte"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi écrire un fichier texte en Gleam?

Il y a de nombreuses raisons pour lesquelles vous pourriez vouloir écrire un fichier texte en utilisant le langage de programmation Gleam. Cela peut inclure la manipulation de données, la création de rapports ou la mise en forme de contenu pour une application Web. Quelle que soit la raison, écrire un fichier texte en Gleam peut vous aider à automatiser des tâches et à améliorer votre workflow.

## Comment faire?

Pour écrire un fichier texte en Gleam, vous pouvez utiliser la fonction `write_file` de la bibliothèque standard `std/fs`. Cette fonction prend deux arguments: le chemin vers le fichier à écrire et le contenu du fichier sous forme de chaîne de caractères. Voici un exemple de code pour écrire un fichier texte contenant le message "Bonjour, monde!" :

```
Gleam program
pub fn main() {
  let content = "Bonjour, monde!";
  let _ = std/fs.write_file("hello.txt", content);
}
```

Une fois que vous avez exécuté ce code, vous devriez voir le fichier `hello.txt` créé dans le même répertoire que votre programme Gleam.

## Plongez plus profondément

Outre l'utilisation de la fonction `write_file`, vous pouvez également utiliser d'autres bibliothèques ou fonctions pour manipuler le contenu de votre fichier texte. Par exemple, vous pouvez utiliser la bibliothèque `std/encoding` pour formater votre texte en utilisant différents encodages tels que UTF-8 ou ASCII. Vous pouvez également utiliser la fonction `append` de la bibliothèque `std/fs` pour ajouter du contenu à un fichier existant plutôt que de l'écraser complètement.

## Voir aussi

- Documentation officielle de Gleam: https://gleam.run/documentation/
- Guide de démarrage rapide de Gleam: https://gleam.run/getting-started/
- Exemples de code Gleam: https://github.com/gleam-lang/gleam/tree/master/examples