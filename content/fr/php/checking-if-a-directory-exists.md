---
title:    "PHP: Vérifier l'existence d'un répertoire"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

#Pourquoi

Vérifier l'existence d'un répertoire dans une application PHP est une étape importante pour s'assurer que le code fonctionne correctement. Cela peut être utile pour créer un nouveau dossier si nécessaire ou pour éviter des erreurs lors de l'utilisation d'un fichier stocké dans un répertoire spécifique.

#Comment Faire

L'utilisation de la fonction `is_dir()` en PHP permet de vérifier si un répertoire existe déjà. Voici un exemple de code PHP pour vérifier l'existence d'un répertoire nommé "images" dans le répertoire actuel :

```PHP
if(is_dir('images')){
  echo "Le répertoire existe déjà.";
}else{
  echo "Le répertoire n'existe pas encore.";
}
```

Si le répertoire existe, le message "Le répertoire existe déjà." sera affiché. Sinon, le message "Le répertoire n'existe pas encore." sera affiché.

Vous pouvez également utiliser cette fonction pour créer un nouveau répertoire si nécessaire. Voici un exemple de code qui vérifie d'abord si le répertoire existe et, s'il n'existe pas, le crée :

```PHP
if(!is_dir('documents')){
  mkdir('documents');
  echo "Le répertoire a été créé.";
}else{
  echo "Le répertoire existe déjà.";
}
```

Dans cet exemple, si le répertoire "documents" n'existe pas encore, il sera créé et le message "Le répertoire a été créé." sera affiché.

#Plongée en Profondeur

Il peut être utile de comprendre comment la fonction `is_dir()` vérifie si un répertoire existe. En réalité, cette fonction utilise la fonction `file_exists()` pour vérifier si le chemin spécifié correspond à un fichier ou à un répertoire existant. Si le chemin existe et correspond à un répertoire, la fonction `is_dir()` renvoie "vrai" (true). Sinon, elle renvoie "faux" (false).

Il est également important de noter que cette fonction ne vérifie que l'existence d'un répertoire et non sa permission d'accès. Cela signifie qu'elle peut renvoyer "vrai" même si vous n'avez pas la permission d'accéder au répertoire.

#Voir Aussi

- [Documentation officielle de la fonction is_dir() en PHP](https://www.php.net/manual/fr/function.is-dir.php)
- [Documentation officielle de la fonction file_exists() en PHP](https://www.php.net/manual/fr/function.file-exists.php)
- [Vérification de l'existence d'un fichier en PHP](https://www.php.net/manual/fr/function.file-exists.php)