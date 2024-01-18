---
title:                "Comparer deux dates"
html_title:           "Lua: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Comparer deux dates en programmation consiste à vérifier si une date est antérieure, égale ou postérieure à une autre date. Les programmeurs le font souvent pour trier des données chronologiquement ou pour effectuer des opérations basées sur la date. 

## Comment faire?

Voici un exemple de code en Lua pour comparer deux dates:

```Lua
-- Définition de deux dates sous forme de tableaux
date1 = {jour = 25, mois = 03, année = 2021} 
date2 = {jour = 02, mois = 06, année = 2019}

-- Création d'une fonction pour comparer les dates
function comparerDates(date1, date2)
	-- Si l'année de la première date est supérieure alors c'est une date plus récente
	if date1.année > date2.année then
		return "La date 1 est plus récente que la date 2"
	-- Si l'année de la première date est inférieure alors c'est une date plus ancienne
	elseif date1.année < date2.année then
		return "La date 1 est plus ancienne que la date 2"
	-- Si les années sont égales, on compare les mois
	elseif date1.mois > date2.mois then
		return "La date 1 est plus récente que la date 2"
	elseif date1.mois < date2.mois then
		return "La date 1 est plus ancienne que la date 2"
	-- Si les mois sont égaux, on compare les jours
	elseif date1.jour > date2.jour then
		return "La date 1 est plus récente que la date 2"
	elseif date1.jour < date2.jour then
		return "La date 1 est plus ancienne que la date 2"
	-- Si toutes les valeurs sont égales, alors les dates sont identiques
	else
		return "Les dates sont identiques"
	end
end

-- Appel de la fonction et affichage du résultat
print(comparerDates(date1, date2))
```
Résultat: La date 1 est plus récente que la date 2

## Plongée en profondeur

Dans le passé, les programmeurs devaient souvent écrire du code complexe pour comparer deux dates, notamment lorsqu'il s'agissait de gérer les années bissextiles. Heureusement, avec les langages de programmation modernes comme Lua, il existe des fonctions intégrées pour faciliter la comparaison de dates, telles que `os.time()` qui renvoie le nombre de secondes écoulées depuis le 1er janvier 1970. Cela permet de comparer facilement des dates en les convertissant d'abord en secondes.

Il existe également d'autres méthodes pour comparer des dates, comme utiliser des librairies externes ou des fonctions de calcul de différence de temps. Il est important de choisir une méthode qui convient le mieux à votre cas d'utilisation et de la tester soigneusement pour éviter les erreurs.

## À voir également

- Documentation officielle de Lua pour en savoir plus sur les fonctions intégrées pour la manipulation de dates : https://www.lua.org/manual/5.4/manual.html#6.9
- Librairies externes telles que LuaDate qui offrent des fonctionnalités avancées pour la gestion de dates en Lua : http://lua-users.org/wiki/LuaModulesDateTime