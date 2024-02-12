---
title:                "Analyse Syntaxique du HTML"
aliases:
- /fr/c-sharp/parsing-html.md
date:                  2024-02-03T19:11:46.556508-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyse Syntaxique du HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

L'analyse syntaxique du HTML en programmation implique l'analyse de la structure d'un document HTML, ce qui vous permet d'extraire, de manipuler et d'interagir programmatiquement avec son contenu. Les programmeurs font cela pour automatiser le web scraping, l'extraction de données, ou même modifier des pages web ou des documents HTML dynamiquement pour diverses applications, ce qui en fait une compétence essentielle dans le développement web, l'analyse de données et les scénarios de tests automatisés.

## Comment faire :

Bien que .NET offre un support de base pour travailler avec le HTML, comme le `HttpClient` pour récupérer des pages web, il lui manque un analyseur HTML intégré et complet. Par conséquent, la plupart des développeurs C# se tournent vers des bibliothèques tierces populaires comme HtmlAgilityPack ou AngleSharp pour des capacités robustes d'analyse syntaxique du HTML. Ces deux bibliothèques permettent une interrogation, une manipulation et une traversée faciles du DOM HTML.

### Utilisation de HtmlAgilityPack

1. **Installer HtmlAgilityPack** : D'abord, ajoutez le package HtmlAgilityPack à votre projet via NuGet.
   ```
   Install-Package HtmlAgilityPack
   ```

2. **Code exemple** : Analyser une chaîne HTML et extraire les titres de tous les éléments `<h1>`.

   ```csharp
   using HtmlAgilityPack;
   using System;
   using System.Linq;

   class Program
   {
       static void Main(string[] args)
       {
           var html = @"<html>
                         <body>
                             <h1>Titre 1</h1>
                             <h1>Titre 2</h1>
                         </body>
                        </html>";
           var htmlDoc = new HtmlDocument();
           htmlDoc.LoadHtml(html);

           var h1Tags = htmlDoc.DocumentNode.SelectNodes("//h1").Select(node => node.InnerText);
           foreach (var title in h1Tags)
           {
               Console.WriteLine(title);
           }
       }
   }
   ```

   **Exemple de sortie :**
   ```
   Titre 1
   Titre 2
   ```

### Utilisation de AngleSharp

1. **Installer AngleSharp** : Ajoutez la bibliothèque AngleSharp à votre projet via NuGet.
   ```
   Install-Package AngleSharp
   ```

2. **Code exemple** : Charger un document HTML et interroger les éléments `div` ayant une classe spécifique.

   ```csharp
   using AngleSharp;
   using AngleSharp.Dom;
   using System;
   using System.Linq;
   using System.Threading.Tasks;

   class Program
   {
       static async Task Main(string[] args)
       {
           var context = BrowsingContext.New(Configuration.Default);
           var document = await context.OpenAsync(req => req.Content("<div class='item'>Élément 1</div><div class='item'>Élément 2</div>"));

           var items = document.QuerySelectorAll(".item").Select(element => element.TextContent);
           foreach (var item in items)
           {
               Console.WriteLine(item);
           }
       }
   }
   ```

   **Exemple de sortie :**
   ```
   Élément 1
   Élément 2
   ```

HtmlAgilityPack et AngleSharp sont tous deux des outils puissants pour l'analyse syntaxique du HTML, mais votre choix entre eux pourrait dépendre de besoins spécifiques de projet, de considérations de performance ou de préférences personnelles dans la conception de l'API.
