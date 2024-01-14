---
title:    "C#: Utilizzare le espressioni regolari"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore che si occupa spesso di elaborazione di testo o dati, potresti aver sentito parlare delle espressioni regolari o "regular expressions" in inglese. Questo strumento può sembrare un po' intimidatorio all'inizio, ma imparare a utilizzarlo può semplificare notevolmente il tuo lavoro e risparmiare tempo nella ricerca e manipolazione di testi e dati.

## Come usare le espressioni regolari in C#

Per utilizzare le espressioni regolari in C#, è necessario utilizzare il namespace "System.Text.RegularExpressions". All'interno di questo namespace, è disponibile la classe Regex che fornisce diversi metodi per lavorare con le espressioni regolari.

Per creare una nuova istanza di Regex, è possibile utilizzare il costruttore che prende come parametro la stringa della regex. Ad esempio:

```C#
Regex regex = new Regex(@"\d{2}/\d{2}/\d{4}");
```

In questo esempio, abbiamo creato una regex per cercare corrispondenze di un formato di data, ovvero due numeri per il giorno, due per il mese e quattro per l'anno, separati da slash ("/").

Per testare se una stringa corrisponde alla regex, è possibile utilizzare il metodo Match() della classe Regex, passando come parametro la stringa da controllare. Ad esempio:

```C#
string testo = "La data di oggi è 10/02/2021";
Match match = regex.Match(testo);

if(match.Success)
{
    Console.WriteLine("La data è valida!");
    Console.WriteLine("Giorno: " + match.Groups[1].Value);
    Console.WriteLine("Mese: " + match.Groups[2].Value);
    Console.WriteLine("Anno: " + match.Groups[3].Value);
}
```

In questo caso, il metodo Match() restituirà un oggetto Match contenente le informazioni sulle corrispondenze trovate. Nel nostro esempio, il metodo Success restituirà il valore "true" poiché la data fornita è valida, e accederemo ai gruppi di corrispondenza tramite la proprietà Groups, indicizzando in base alla posizione degli espressioni tra parentesi nella regex.

## Approfondimento sull'utilizzo delle espressioni regolari

Le espressioni regolari sono molto più di un semplice strumento per trovare corrispondenze in una stringa. Sono un linguaggio a sé stante, con un set di regole e costrutti che consentono di creare pattern molto specifici per trovare e manipolare testi e dati.

Con le espressioni regolari è possibile cercare non solo corrispondenze perfette, ma anche corrispondenze approssimative tramite l'utilizzo di metacaratteri come il punto (".") che indica qualsiasi carattere, o l'asterisco ("*") che indica che quel carattere o gruppo può apparire 0 o più volte. Inoltre, è possibile utilizzare le parentesi per raggruppare porzioni della regex e accedere a esse tramite i gruppi di corrispondenza.

Il mondo delle espressioni regolari è vasto e può sembrare un po' complesso all'inizio, ma una volta padroneggiato può diventare uno strumento potente ed efficiente per il tuo lavoro di programmazione.

## Vedi anche

- [Documentazione ufficiale di Microsoft sulle espressioni regolari in C#](https://docs.microsoft.com/it-it/dotnet/standard/base-types/regular-expressions)
- [Tutorial sulle espressioni regolari in C# su Codecademy](https://www.codecademy.com/learn/learn-c-sharp/modules/learn-csharp-regular-expressions)
- [10 esempi di espressioni regolari in C#](https://www.c-sharpcorner.com/article/ten-regular-expression-example-in-C-Sharp/)