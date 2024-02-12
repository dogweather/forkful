---
title:                "Arrotondamento dei numeri"
date:                  2024-02-01T22:00:59.927772-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrotondamento dei numeri"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/vba/rounding-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Nella programmazione, l'arrotondamento dei numeri riguarda l'approssimazione di un numero al suo intero più vicino o a un certo numero di cifre decimali. I programmatori arrotondano i numeri per semplificare le cifre, migliorare la leggibilità o soddisfare criteri numerici specifici nei calcoli, soprattutto nei calcoli finanziari dove la precisione è fondamentale.

## Come fare:

In Visual Basic for Applications (VBA), l'arrotondamento può essere ottenuto utilizzando diverse funzioni, ognuna adatta a scenari specifici. Ecco le funzioni più comunemente usate con degli esempi:

1. **Funzione Round**:
   La funzione `Round` arrotonda un numero a un numero specificato di cifre.
   ```basic
   Dim roundedNumber As Double
   roundedNumber = Round(3.14159, 2)  ' Output: 3.14
   MsgBox roundedNumber
   ```
   
2. **Funzioni Int e Fix**:
   Le funzioni `Int` e `Fix` sono usate per arrotondare i numeri all'intero più vicino verso il basso, ma si comportano diversamente con i numeri negativi.
   ```basic
   Dim intRounded As Integer
   Dim fixRounded As Integer
   
   intRounded = Int(-3.14159)  ' Output: -4
   fixRounded = Fix(-3.14159)  ' Output: -3
   
   MsgBox "Int: " & intRounded & ", Fix: " & fixRounded
   ```

3. **Funzioni Ceiling e Floor**:
   VBA non dispone delle funzioni `Ceiling` e `Floor` integrate trovate in altri linguaggi. Per simulare ciò, usare `Application.WorksheetFunction.Ceiling_Math` e `Application.WorksheetFunction.Floor_Math` per Excel VBA.
   ```basic
   Dim ceilingNumber As Double
   Dim floorNumber As Double
   
   ceilingNumber = Application.WorksheetFunction.Ceiling_Math(3.14159)  ' Output: 4
   floorNumber = Application.WorksheetFunction.Floor_Math(3.14159)  ' Output: 3
   
   MsgBox "Ceiling: " & ceilingNumber & ", Floor: " & floorNumber
   ```

## Approfondimento

La funzione `Round` in VBA è intrinsecamente diversa dai metodi di arrotondamento in altri linguaggi a causa del suo uso dell'**Arrotondamento Bancario**. L'Arrotondamento Bancario arrotonda al numero pari più vicino quando si trova esattamente a metà tra due numeri, riducendo il bias nei calcoli su un grande set di dati e fornendo un risultato statisticamente più significativo. Tuttavia, ciò può portare a comportamenti inaspettati per chi non ne è a conoscenza, specialmente quando si prevede precisione integrale in ogni caso.

Al contrario, molti linguaggi di programmazione e sistemi usano l'"arrotondamento aritmetico" o "arrotondamento verso l'alto", dove un numero esattamente a metà tra due possibili valori arrotondati viene sempre arrotondato verso l'alto. Quando si traduce o si porta codice da altri linguaggi a VBA, i programmatori devono tenere presenti queste differenze per evitare sottili bug o inesattezze nelle applicazioni finanziarie e statistiche.

Sebbene VBA offra una varietà di funzioni per l'arrotondamento, l'assenza delle funzioni `Ceiling` e `Floor` (senza ricorrere a WorksheetFunction di Excel) evidenzia una limitazione nelle sue capacità native. I programmatori provenienti da linguaggi più ricchi di funzionalità potrebbero trovare queste omissioni scomode e potrebbero aver bisogno di implementare soluzioni personalizzate o adattare i loro calcoli per utilizzare le funzioni disponibili. Nonostante queste limitazioni, comprendere e utilizzare correttamente le funzioni di arrotondamento di VBA può aiutare a garantire che i calcoli numerici siano sia accurati che rispondano ai requisiti della maggior parte delle applicazioni.
