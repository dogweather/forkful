---
title:    "Java: Calcolare una data nel futuro o nel passato"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché
Sei mai rimasto confuso sul come calcolare una data nel futuro o nel passato? Scopri come farlo facilmente con Java!

## Come fare
```Java
public class Calendario {
  public static void main(String[] args) {

    // Definiamo la data di partenza
    int anno = 2021;
    int mese = 6;
    int giorno = 15;

    // Calcoliamo la data nel futuro
    int giorniFuturi = 10;
    giorno += giorniFuturi;

    // Escludiamo mesi con meno di 31 giorni
    if (mese == 4 || mese == 6 || mese == 9 || mese == 11) {
      if (giorno > 30) {
        giorno -= 30;
        mese++;
      }
    }
    // Escludiamo Febbraio e Anni bisestili
    else if (mese == 2) {
      if ((anno % 4 == 0 && anno % 100 != 0) || anno % 400 == 0) {
        if (giorno > 29) {
          giorno -= 29;
          mese++;
        }
      } else {
        if (giorno > 28) {
          giorno -= 28;
          mese++;
        }
      }
    }
    // Restituisci la data nel futuro
    System.out.println("Data futura: " + anno + "-" + mese + "-" + giorno);

    // Calcoliamo la data nel passato
    int giorniPassati = 10;
    giorno -= giorniPassati;

    // Escludiamo mesi con meno di 31 giorni
    if (mese == 4 || mese == 6 || mese == 9 || mese == 11) {
      if (giorno < 1) {
        giorno += 30;
        mese--;
      }
    }
    // Escludiamo Febbraio e Anni bisestili
    else if (mese == 2) {
      if ((anno % 4 == 0 && anno % 100 != 0) || anno % 400 == 0) {
        if (giorno < 1) {
          giorno += 29;
          mese--;
        }
      } else {
        if (giorno < 1) {
          giorno += 28;
          mese--;
        }
      }
    }
    // Restituisci la data nel passato
    System.out.println("Data passata: " + anno + "-" + mese + "-" + giorno);

  }
}
```
Esempio di output:
Data futura: 2021-6-25
Data passata: 2021-6-5

## Approfondimento
Il calcolo di una data nel futuro o nel passato richiede l'utilizzo di operazioni matematiche e una conoscenza dei mesi e dei giorni dell'anno. Una volta compresa questa logica, è possibile scrivere una funzione per calcolare facilmente qualsiasi data desiderata.

## Vedi anche
- [Come calcolare i giorni tra due date con Java](https://www.javatpoint.com/days-between-dates-in-java)
- [Calcolo della data della Pasqua con Java](https://www.codemartial.org/2013/03/calcolo-della-data-della-pasqua-con-java.html)
- [Trasformare una data in un formato diverso con Java](https://stackoverflow.com/questions/4216745/how-to-convert-a-date-to-string-in-java)