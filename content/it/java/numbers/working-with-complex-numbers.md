---
title:                "Lavorare con i numeri complessi"
aliases: - /it/java/working-with-complex-numbers.md
date:                  2024-01-26T04:41:53.714586-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i numeri complessi"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

I numeri complessi estendono la linea dei numeri reali attraverso l'aggiunta di un'unità immaginaria, `i`, dove `i^2 = -1`. Sono fondamentali in campi come l'ingegneria, la fisica e la matematica avanzata, dove modellano fenomeni che i numeri reali non possono gestire, come le correnti elettriche e l'elaborazione dei segnali.

## Come fare:

Java non ha un supporto incorporato per i numeri complessi, ma possiamo creare una nostra classe o usare una libreria. Ecco un esempio rapido di come creare una semplice classe `ComplexNumber` e utilizzarla:

```java
public class ComplexNumber {
    private double reale;
    private double immaginario;

    public ComplexNumber(double reale, double immaginario) {
        this.reale = reale;
        this.immaginario = immaginario;
    }

    public ComplexNumber add(ComplexNumber altro) {
        return new ComplexNumber(this.reale + altro.reale, this.immaginario + altro.immaginario);
    }

    // ToString per visualizzare i numeri complessi in forma a + bi
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", reale, immaginario);
    }

    // Test rapido
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("Somma: " + c1.add(c2));
    }
}
```

Il risultato dell'esecuzione del metodo main sarà:

```
Somma: 3.0 + 7.0i
```

## Approfondimento

Prima dei linguaggi ad alto livello come Java, i programmatori lavoravano direttamente con le librerie matematiche in linguaggi come Fortran o C per gestire operazioni complesse. Il concetto risale al XVI secolo, accreditato a matematici come Gerolamo Cardano e Rafael Bombelli.

In Java, `java.lang.Math` è la scelta principale per l'essenziale ma omette i numeri complessi, probabilmente perché non tutti i programmatori li usano. Alternative? Utilizzare le librerie. Apache Commons Math fornisce una classe `Complex` ricca di metodi per la manipolazione. Ecco perché però creare la propria può essere interessante: Leggerezza, su misura per le proprie esatte necessità e nessun sovraccarico da librerie.

Un dettaglio importante: attenzione alla precisione dei numeri in virgola mobile. I computer non possono rappresentare alcuni numeri esattamente, portando a errori di arrotondamento. Eseguendo operazioni complesse ripetitive, questi errori possono accumularsi!

## Vedere Anche

Per approfondimenti e operazioni più complesse, controllare:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [La classe Complex di JScience](http://jscience.org/)
- I tutorial di Oracle sull'[aritmetica in virgola mobile](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
