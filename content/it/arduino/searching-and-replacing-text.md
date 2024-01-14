---
title:    "Arduino: Ricerca e sostituzione di testo"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Perché

Il processo di ricerca e sostituzione del testo è un'attività comune e importante nella programmazione di Arduino. Questa tecnica consente di salvare tempo e di effettuare modifiche in modo efficace all'interno del codice.

## Come fare

Per iniziare, apri il tuo ambiente di sviluppo Arduino e crea un nuovo sketch. Utilizziamo come esempio uno sketch che stampa una serie di numeri da 1 a 10 sulla console seriale.

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  for(int i=1; i<=10; i++) {
    Serial.println(i);
  }
  delay(1000);
}
```

Supponiamo che vogliamo sostituire tutti i numeri pari con la parola "ciao". Utilizzando la funzione `replace()` possiamo facilmente raggiungere questo scopo.

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  for(int i=1; i<=10; i++) {
    if(i % 2 == 0) {
      String new_num = "ciao";
      Serial.println(new_num);
    } else {
      Serial.println(i);
    }
  }
  delay(1000);
}
```

Il codice sopra eseguirà in modo efficace la ricerca e la sostituzione dei numeri pari all'interno della nostra stampa su console seriale.

## Approfondimenti

Oltre alla sostituzione di testo semplice, è possibile utilizzare la funzione `replace()` per effettuare sostituzioni condizionali, ad esempio sostituendo solo alcune occorrenze di un determinato testo o utilizzando espressioni regolari per effettuare sostituzioni più complesse e precise.

## Vedi anche

- [Documentazione ufficiale di Arduino](https://www.arduino.cc/reference/it/language/variables/data-types/string/functions/replace/)
- [Tutorial su ricerca e sostituzione di testo in Arduino](https://www.hackster.io/projects/search?keywords=search%20and%20replace&sort=trending&type=all)