---
title:    "C#: Vérifier l'existence d'un répertoire"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

L'existence d'un répertoire peut être essentielle dans la programmation en C#. Dans cet article, nous allons explorer pourquoi il est important de vérifier si un répertoire existe et comment le faire de manière efficace.

## Comment faire

Pour vérifier si un répertoire existe en C#, nous pouvons utiliser la méthode Directory.Exists. Cette méthode renvoie un booléen, true si le répertoire existe, false sinon. Regardons un exemple pour mieux comprendre:

```C#
string chemin = @"C:\Users\Utilisateur\Desktop\MonDossier";
if(Directory.Exists(chemin))
{
    Console.WriteLine("Le répertoire existe !");
}
else
{
    Console.WriteLine("Le répertoire n'existe pas...");
}
```

Si le répertoire "MonDossier" existe sur le bureau de l'utilisateur, alors l'output sera "Le répertoire existe !". 

## Plongée en profondeur

Il est important de noter que la méthode Directory.Exists peut retourner false même si le répertoire existe réellement. Cela peut se produire si l'utilisateur n'a pas les permissions nécessaires pour accéder au répertoire. Dans ce cas, une exception UnauthorizedAccessException sera levée.

Si vous souhaitez en savoir plus sur les raisons pour lesquelles un répertoire peut ne pas être accessible ou pour apprendre comment gérer ces exceptions, je vous recommande de consulter cet article (https://docs.microsoft.com/fr-fr/dotnet/standard/io/handling-io-errors).

## Voir aussi

- [Documentation officielle sur la méthode Directory.Exists](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.directory.exists)
- [Gérer les exceptions d'accès aux fichiers et répertoires en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/io/handling-io-errors)