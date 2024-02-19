---
aliases:
- /fr/vba/working-with-complex-numbers/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:50.216203-07:00
description: "Travailler avec des nombres complexes implique d'effectuer des op\xE9\
  rations math\xE9matiques sur des nombres ayant \xE0 la fois une partie r\xE9elle\
  \ et une partie\u2026"
lastmod: 2024-02-18 23:09:08.570997
model: gpt-4-0125-preview
summary: "Travailler avec des nombres complexes implique d'effectuer des op\xE9rations\
  \ math\xE9matiques sur des nombres ayant \xE0 la fois une partie r\xE9elle et une\
  \ partie\u2026"
title: Travailler avec des nombres complexes
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Travailler avec des nombres complexes implique d'effectuer des opérations mathématiques sur des nombres ayant à la fois une partie réelle et une partie imaginaire. Les programmeurs s'engagent souvent avec des nombres complexes dans des domaines tels que l'ingénierie, la physique, et partout où cela implique de résoudre des équations qui ne sont pas possibles avec de simples nombres réels.

## Comment faire :

Dans Visual Basic pour Applications (VBA), la manipulation de nombres complexes peut être quelque peu moins directe par rapport à des langues avec un support natif pour eux. Cependant, vous pouvez gérer des opérations complexes en créant des fonctions ou en utilisant des fonctions de bibliothèque existantes. Explorons un exemple de base d'addition, de soustraction, de multiplication et de division de nombres complexes :

```vb
' Fonction pour additionner des nombres complexes
Function AddComplex(x As String, y As String) As String
    Dim real1 As Double, imag1 As Double
    Dim real2 As Double, imag2 As Double
    
    ' Extraction des parties réelle et imaginaire des nombres complexes
    real1 = Val(Split(x, "+")(0))
    imag1 = Val(Split(x, "+")(1))
    real2 = Val(Split(y, "+")(0))
    imag2 = Val(Split(y, "+")(1))
    
    ' Effectuer l'addition
    AddComplex = (real1 + real2) & "+" & (imag1 + imag2) & "i"
End Function

' Exemple d'utilisation
Sub ExempleUtilisation()
    Dim result As String
    result = AddComplex("3+2i", "1+7i")
    Debug.Print "Résultat de l'Addition : " & result  ' Sortie : Résultat de l'Addition : 4+9i
End Sub
```

Bien que cela démontre l'addition, des approches similaires peuvent être adaptées pour la soustraction, la multiplication et la division. Pour des opérations complexes au-delà de l'arithmétique de base, il peut être utile d'explorer des bibliothèques externes ou d'intégrer d'autres solutions qui supportent les opérations de nombres complexes plus nativement.

## Plongée en profondeur :

VBA n'inclut pas de support intégré pour les nombres complexes, un aspect où il est en retard par rapport à des langues comme Python, qui dispose d'une classe de nombre complexe (`complex`) ou C++ avec sa Standard Template Library (`std::complex`). Historiquement, le besoin de manipuler directement des nombres complexes dans VBA est relativement rare, car il est souvent utilisé pour l'automatisation, manipulant des applications Office, et des tâches qui traditionnellement ne nécessitent pas de calculs mathématiques complexes. Lorsque VBA a été conçu et développé, ses cas d'utilisation étaient principalement axés sur les applications commerciales plutôt que le calcul scientifique, ce qui pourrait expliquer cette omission.

Pour des tâches qui requièrent des manipulations de nombres complexes étendues, les programmeurs pourraient trouver avantageux d'utiliser une langue plus orientée vers les mathématiques. Cependant, pour ceux qui sont engagés ou limités par l'utilisation de VBA, écrire des fonctions personnalisées (comme illustré) ou intégrer avec des logiciels qui ont ces capacités (tel que MATLAB ou Excel lui-même jusqu'à un certain point) sont des voies viables à suivre. Malgré ses limitations, des solutions créatives et des intégrations externes peuvent étendre l'utilité de VBA dans des domaines pour lesquels il n'était pas initialement conçu, y compris le travail avec des nombres complexes.
