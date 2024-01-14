---
title:    "C++: Concatenare stringhe"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è una tecnica essenziale nella programmazione di C ++ che consente di unire diverse stringhe per formarne una sola. Questo può essere utile per creare messaggi personalizzati, formattare output o per svolgere altre operazioni che richiedono una combinazione di testo.

## Come Fare

Per concatenare stringhe in C ++, è necessario utilizzare l'operatore di somma "+" tra due stringhe. Ad esempio:

```C++
string stringa1 = "Ciao";
string stringa2 = "mondo";
stringa1 += stringa2;
cout << stringa1;
```

In questo esempio, la variabile "stringa1" contiene la stringa "Ciao" e la variabile "stringa2" contiene la stringa "mondo". Utilizzando l'operatore "+=" d'assegnazione, le due stringhe vengono concatenate insieme e il risultato viene stampato su schermo come "Ciaomondo".

Si possono anche concatenare più stringhe in una volta sola. Ad esempio:

```C++
string stringa1 = "Ciao";
string stringa2 = "mondo";
string stringa3 = "!";
cout << stringa1 + " " + stringa2 + stringa3;
```

In questo caso, il risultato stampato sarà "Ciao mondo!" poiché le tre stringhe sono state concatenate in un'unica istruzione.

## Approfondimento

Per unire più di due stringhe, si può utilizzare la funzione "append()" insieme all'operatore "+". Ad esempio:

```C++
string stringa1 = "Benvenuti";
string stringa2 = "nel";
string stringa3 = "mondo";
stringa1.append(" " + stringa2).append(" " + stringa3);
cout << stringa1;
```

In questo caso, l'output stampato sarà "Benvenuti nel mondo", in quanto la funzione "append()" consente di aggiungere una stringa alla fine di un'altra.

È importante anche considerare la performance durante l'uso della concatenazione di stringhe. Poiché in C ++ le stringhe sono oggetti, possono essere costose da gestire se ne sono presenti molte e di lunghezza notevole. In questi casi, potrebbe essere più efficiente utilizzare un'altra tecnica come la classe "stringstream", che permette di costruire una stringa un pezzo alla volta senza dover creare oggetti intermedi.

## Vedi Anche

- [Documentazione ufficiale di C++ sulla concatenazione di stringhe](http://www.cplusplus.com/reference/string/string/operator+=/)
- [Tutorial su stringstream in C++](https://www.geeksforgeeks.org/stringstream-c-applications/)