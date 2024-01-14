---
title:                "Go: Analyse de l'html"
simple_title:         "Analyse de l'html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeur en Go, vous avez peut-être entendu parler du terme "parsing HTML" ou "analyse HTML". Mais pourquoi est-il important de savoir comment le faire ? Tout simplement parce que le HTML est la langue dans laquelle les pages web sont écrites. Ainsi, il est essentiel de pouvoir comprendre et extraire les données de ces pages pour créer des applications web ou pour utiliser des données pour d'autres projets.

## Comment faire

Pour effectuer une analyse HTML en Go, vous aurez besoin d'une bibliothèque externe appelée "goquery". Pour l'installer, vous pouvez utiliser la commande "go get" :

```Go
go get github.com/PuerkitoBio/goquery
```

Ensuite, vous pouvez importer la bibliothèque dans votre code en ajoutant cette ligne :

```Go
import "github.com/PuerkitoBio/goquery"
```

Maintenant, vous pouvez commencer à utiliser les fonctions de la bibliothèque pour extraire les données HTML souhaitées. Voici un exemple de code qui extrait le titre et le lien de chaque article sur la page d'accueil de HuffPost :

```Go
doc, err := goquery.NewDocument("https://www.huffingtonpost.fr/")
if err != nil {
    log.Fatal(err)
}

doc.Find(".card__content").Each(func(i int, s *goquery.Selection) {
	title := s.Find(".card__title").Text()
	link, _ := s.Find(".card__link").Attr("href")
	fmt.Printf("%d. %s - %s\n", i+1, title, link)
})
```

Et voici ce que le code ci-dessus produirait :

```Go
1. La police française travaille sur une vidéo porno gay tournée dans un commissariat - https://www.huffingtonpost.fr/entry/handicap-inclusif-travail_fr_60c32734e4b0b7e5e2f3a0c6
2. "On ne fera pas l'Europe sans l'UE" : comment la gauche change de discours - https://www.huffingtonpost.fr/entry/interview-francais-souverainete-europe-populisme-galzin-urvoas_fr_60c1ae82e4b0f973238a646c
3. "The Father" : la lutte pour comprendre Anthony Hopkins - https://www.huffingtonpost.fr/entry/the-father-anthony-hopkins-fr_an_60c34a6ae4b0b7e5e2f4311f
...
```

## Plongée en profondeur

Maintenant que vous savez comment utiliser la bibliothèque goquery pour effectuer une analyse HTML, il est temps de plonger en profondeur dans le fonctionnement du parsing HTML. En général, cela implique de créer un "arbre" de noeuds HTML à partir du code source de la page web, puis de naviguer à travers cet arbre pour trouver les éléments HTML souhaités. Cela peut sembler compliqué, mais heureusement, des bibliothèques telles que goquery simplifient grandement le processus en vous permettant de rechercher des éléments HTML spécifiques à l'aide de sélecteurs CSS.

## Voir aussi

Pour en savoir plus sur le parsing HTML en Go, voici quelques liens utiles :

- [Documentation officielle de goquery](https://github.com/PuerkitoBio/goquery)
- [Guide pratique pour l'analyse HTML en Go](http://go-colly.org/articles/scraping_with_golang/)
- [Tutoriel vidéo sur le parsing HTML en Go](https://www.youtube.com/watch?v=6lEaIOJ9V2U)

Maintenant que vous avez les connaissances de base pour effectuer une analyse HTML en Go, vous pouvez commencer à créer vos propres outils pour extraire des données à partir de pages web ou à utiliser ces données pour des projets passionnants. Bonne programmation !