---
title:                "Tarkistetaan, onko kansio olemassa"
html_title:           "Elm: Tarkistetaan, onko kansio olemassa"
simple_title:         "Tarkistetaan, onko kansio olemassa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Kun teet ohjelmoijana, on tärkeää tietää, miten tarkistaa, onko kansio olemassa . Tämä auttaa varmistamaan, että koodisi toimii sujuvasti ja välttämään virheitä.

## Kuinka tehdä

#### Elm haasteen

```Elm
-- Tämä funktio tarkistaa, onko annetun polun kansio olemassa
checkDirectoryExists : String -> Bool
checkDirectoryExists path =
	let
		result = 
			case path of
				"/" ->
					True

				_ ->
					case String.split "/" path of
						"." :: rest ->
							checkDirectoryExists (String.join "/" rest)

						[a, b] ->
							isDirectory a b

						_ ->
							False
	in
		if result then
			True

		else
			case String.split "/" path of
				["", a] ->
					isDirectory "/" a

				[root, a] ->
					isDirectory ("/" ++ root) a

				other ->
					False
```

Tässä esimerkissä käytetään Elm funktiota nimeltä "checkDirectoryExists", joka tarkistaa, onko annettu polku olemassa oleva kansio. Funktio käyttää tapauslausekkeita käsittelemään erilaisia mahdollisia polkuja ja palauttaa lopuksi "True" tai "False" arvon. Voit käyttää tätä funktiota omassa koodissasi varmistaaksesi, että kansiot ovat olemassa ennen kuin yrität kirjoittaa tai lukea niitä.

#### Esimerkki

```Elm
-- Tarkista, onko kansion "documents" polku olemassa
checkDirectoryExists "home/documents" -- palauttaa "True"

-- Tarkista, onko kansion "pictures" polku olemassa
checkDirectoryExists "home/pictures" -- palauttaa "False"
```

## Syventyminen

Kun tarkistat kansion olemassaoloa Elmillä, otat käyttöön erilaisia tapauksia, joita sinun tulee käsitellä. Nämä voivat sisältää ottaa huomioon erilaisia erikoismerkkejä, kuten "/" ja "." ja käsitellä täydellisiä ja osittaisia polkuja. On myös tärkeää huomata, että kansion tarkistaminen ei välttämättä takaa kansion olemassaoloa, sillä se voi olla luotu toisessa kohdassa koodia.

## Katso myös

- [Elm dokumentaatio](https://elm-lang.org/)
- [Elm oppaat ja opetusohjelmat](https://guide.elm-lang.org/)
- [Elm yhteisöfoorumi](https://discourse.elm-lang.org/)