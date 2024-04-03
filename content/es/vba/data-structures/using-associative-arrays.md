---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:20.649805-07:00
description: "C\xF3mo hacerlo: En VBA, el objeto `Dictionary` proporciona funcionalidad\
  \ similar a los arreglos asociativos. Primero debes agregar una referencia al tiempo\u2026"
lastmod: '2024-03-13T22:44:58.885004-06:00'
model: gpt-4-0125-preview
summary: En VBA, el objeto `Dictionary` proporciona funcionalidad similar a los arreglos
  asociativos.
title: Utilizando arreglos asociativos
weight: 15
---

## Cómo hacerlo:
En VBA, el objeto `Dictionary` proporciona funcionalidad similar a los arreglos asociativos. Primero debes agregar una referencia al tiempo de ejecución de scripting de Microsoft para usarlo:

1. En el editor de VBA, ve a Herramientas > Referencias...
2. Marca "Tiempo de ejecución de scripting de Microsoft" y haz clic en Aceptar.

Así es como se declara, se llena y se accede a los elementos en un `Dictionary`:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' Agregando elementos
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Engineer"

' Accediendo a elementos
Debug.Print sampleDictionary.Item("Name")  ' Salida: John Doe
Debug.Print sampleDictionary.Item("Age")   ' Salida: 29

' Verificando si existe una clave
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Clave Occupation Existe"
End If

' Eliminando elementos
sampleDictionary.Remove("Occupation")

' Recorriendo el diccionario
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## Análisis Profundo
El objeto `Dictionary` se comunica internamente con componentes del Host de Scripts de Windows. Como tal, es un objeto COM ligado tardíamente, lo cual era una forma común de extender la funcionalidad de VBA en el pasado. Su uso en VBA puede mejorar significativamente la capacidad del lenguaje para manipular conjuntos de datos complejos sin imponer una estructura rígida, como se ve en arreglos tradicionales o rangos de Excel.

Una limitación a tener en cuenta es que acceder al `Dictionary` requiere configurar una referencia al Tiempo de Ejecución de Scripting de Microsoft, lo cual puede complicar la distribución de tus proyectos VBA. Alternativas como Colecciones existen dentro de VBA pero carecen de algunas de las características clave del `Dictionary`, como la capacidad de verificar fácilmente la existencia de una clave sin activar un error.

En contextos de programación más recientes, lenguajes como Python ofrecen soporte incorporado para arreglos asociativos (conocidos también como diccionarios en Python) sin necesidad de agregar referencias externas. Este soporte incorporado simplifica el proceso y ofrece características más avanzadas de manera predeterminada. Sin embargo, dentro de los límites de VBA y para aplicaciones específicas orientadas a automatizar tareas en la suite de Microsoft Office, usar el objeto `Dictionary` sigue siendo un método poderoso y relevante para estructuras de datos de tipo arreglo asociativo.
