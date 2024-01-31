---
title:                "Generazione di numeri casuali"
date:                  2024-01-27T20:32:40.947289-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generazione di numeri casuali"

category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

La generazione di numeri casuali in C# comporta la creazione di valori numerici imprevedibili entro un intervallo specificato. I programmatori utilizzano questi metodi per implementare funzionalità come la crittografia, le simulazioni e i giochi dove è richiesta imprevedibilità o la simulazione di casualità nel mondo reale.

## Come fare:

Il modo più comune per generare numeri casuali in C# è utilizzare la classe `System.Random`. Ecco un esempio semplice che ne dimostra l'utilizzo:

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // Genera un numero tra 1 e 99
        Console.WriteLine($"Numero casuale: {randomNumber}");
    }
}
```

Questo produrrà un numero casuale come:

```
Numero casuale: 42
```

Per generare un numero decimale casuale tra 0,0 e 1,0, puoi usare il metodo `NextDouble`:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Doppio casuale: {randomDouble}");
```

Se stai lavorando su un'applicazione sensibile alla sicurezza che richiede casualità crittografica, è meglio utilizzare la classe `RNGCryptoServiceProvider` presente in `System.Security.Cryptography`:

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // Crea un numero casuale lungo 4 byte
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"Numero casuale crittograficamente sicuro: {value}");
    }
}
```

## Approfondimento

La generazione di numeri casuali in C# è evoluta nel corso degli anni. Inizialmente, la classe `System.Random` era la soluzione predefinita per generare numeri pseudo-casuali. È pseudo-casuale perché, dato un valore iniziale specifico, produrrà la stessa sequenza di numeri, il che può essere utile per il debug o la ripetibilità dei test.

Anche se sufficiente per le esigenze basilari, `System.Random` non è thread-safe e può produrre risultati prevedibili, il che non è adatto per applicazioni dipendenti dalla sicurezza. Questa limitazione ha portato all'introduzione del `RNGCryptoServiceProvider` per la casualità crittografica, che è più sicuro ma anche più impegnativo in termini di risorse.

Un'alternativa in .NET Core e .NET 5+ è la classe `RandomNumberGenerator` in `System.Security.Cryptography` per generare numeri casuali in modo sicuro, concepito come un'opzione più moderna e facile da usare rispetto a `RNGCryptoServiceProvider`.

Ogni metodo di generazione di numeri casuali in C# ha il suo posto a seconda delle esigenze dell'applicazione. Per la maggior parte delle applicazioni, `System.Random` è sufficiente, ma per quelle che richiedono numeri casuali sicuri e imprevedibili, le classi crittografiche offrono un'alternativa robusta.
