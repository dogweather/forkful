---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:48.058066-07:00
description: "Comment : Java n'a pas de tableaux associatifs int\xE9gr\xE9s comme\
  \ certains langages, mais il fournit l'interface `Map` et des classes comme `HashMap`\
  \ et\u2026"
lastmod: '2024-03-13T22:44:57.631354-06:00'
model: gpt-4-0125-preview
summary: "Java n'a pas de tableaux associatifs int\xE9gr\xE9s comme certains langages,\
  \ mais il fournit l'interface `Map` et des classes comme `HashMap` et `TreeMap`\
  \ pour remplir ce r\xF4le."
title: Utilisation des tableaux associatifs
weight: 15
---

## Comment :
Java n'a pas de tableaux associatifs intégrés comme certains langages, mais il fournit l'interface `Map` et des classes comme `HashMap` et `TreeMap` pour remplir ce rôle. Voici comment utiliser un `HashMap` :

```Java
import java.util.HashMap;
import java.util.Map;

public class LearnMaps {
    public static void main(String[] args) {
        // Création d'un HashMap
        Map<String, Integer> ageOfFriends = new HashMap<>();
        
        // Ajout d'éléments
        ageOfFriends.put("Alice", 24);
        ageOfFriends.put("Bob", 30);
        ageOfFriends.put("Charlie", 28);

        // Accès aux éléments
        System.out.println("Âge d'Alice : " + ageOfFriends.get("Alice"));
        
        // Gestion des clés inexistantes
        System.out.println("Âge de quelqu'un non présent dans le map : " + ageOfFriends.getOrDefault("Dan", -1));

        // Itération sur les éléments
        for (Map.Entry<String, Integer> entry : ageOfFriends.entrySet()) {
            System.out.println(entry.getKey() + " a " + entry.getValue() + " ans.");
        }
    }
}
```

Exemple de sortie :

```
Âge d'Alice : 24
Âge de quelqu'un non présent dans le map : -1
Alice a 24 ans.
Bob a 30 ans.
Charlie a 28 ans.
```

`HashMap` est seulement une implémentation. Si vos clés sont uniques et que vous avez besoin qu'elles soient triées, considérez `TreeMap`. Pour une map qui retient l'ordre d'insertion, `LinkedHashMap` est votre ami.

## Plongée profonde
Les maps en Java font partie du Framework de Collections, introduit dans JDK 1.2, mais ont vu des améliorations significatives au fil des années, y compris l'introduction de la méthode `forEach` dans Java 8 pour une itération plus facile sur les entrées. Le choix de l'implémentation de map (`HashMap`, `LinkedHashMap`, `TreeMap`) devrait être dicté par vos besoins spécifiques en termes d'ordonnancement et de performance. Par exemple, `HashMap` offre une performance de temps O(1) pour les opérations de base (get et put), en supposant que la fonction de hachage disperse correctement les éléments parmi les seaux. Cependant, si vous avez besoin d'un tri basé sur l'ordre naturel ou des comparateurs personnalisés, `TreeMap` est à privilégier, offrant un temps O(log n) pour l'insertion et la recherche.

Avant l'introduction de `Map`, les tableaux associatifs étaient généralement implémentés avec deux tableaux parallèles (un pour les clés, un pour les valeurs) ou des structures de données personnalisées avec moins d'efficacité. Les alternatives actuelles à `Map` et ses implémentations pourraient inclure des bibliothèques tierces offrant des maps spécialisées, telles que les maps bidirectionnelles (BiMap dans la bibliothèque Guava de Google) pour les cas où vous avez besoin de trouver une clé par sa valeur efficacement. Cependant, pour la plupart des cas d'utilisation en Java, les maps de la bibliothèque standard sont assez robustes et flexibles pour gérer la tâche.
