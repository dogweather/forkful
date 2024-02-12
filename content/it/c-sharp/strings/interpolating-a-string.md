---
title:                "Interpolazione di una stringa"
aliases:
- /it/c-sharp/interpolating-a-string/
date:                  2024-01-20T17:50:39.061408-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolazione di una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
L'interpolazione di stringhe in C# serve a inserire valori di variabili dentro a una stringa di testo. La usiamo perché rende il codice più chiaro e semplice da leggere rispetto alla concatenazione tradizionale.

## How to:
Qui sotto trovi due esempi di come interpolare una stringa. Il primo è semplice, il secondo un po' più tosto.

```C#
// Esempio base
string nome = "Luca";
string saluto = $"Ciao {nome}, come stai?";
Console.WriteLine(saluto); // Output: Ciao Luca, come stai?

// Esempio con espressioni complesse
int x = 5;
double y = 2.5;
string risultato = $"Il prodotto di {x} per {y} è {x * y}.";
Console.WriteLine(risultato); // Output: Il prodotto di 5 per 2.5 è 12.5.
```

## Deep Dive
L'interpolazione di stringhe è arrivata in C# con la versione 6.0, come alternativa alla `String.Format()`. È più intuitiva e veloce da scrivere. Invece di inserire indici nel tuo testo, metti direttamente le variabili tra parentesi graffe, precedute da un segno di dollaro `$`.

Precedentemente, avresti fatto così:

```C#
string nome = "Marco";
string saluto = String.Format("Ciao {0}, come stai?", nome);
```

Con l'interpolazione, il compilatore genera una chiamata a `String.Format()` dietro le quinte, quindi la performance è simile.

Se ti serve una cultura/locale specifica, puoi usare `FormattableString` e `IFormattable`. Così formatti la stringa manualmente:

```C#
FormattableString saluto = $"Ciao {nome}";
string salutoLocalizzato = saluto.ToString(new CultureInfo("it-IT"));
```

Attenzione se stai scrivendo codice per applicazioni con requisiti di sicurezza elevati. Non interpolare mai input non validati, come stringhe inserite dall'utente, perché potresti aprire la porta a vulnerabilità come injection attacks.

## See Also
- Documentazione ufficiale Microsoft sull'interpolazione di stringhe:
  [Interpolazione di stringhe](https://docs.microsoft.com/it-it/dotnet/csharp/language-reference/tokens/interpolated)
- Post sul blog di .NET riguardante le performance dell'interpolazione di stringhe:
  [.NET Blog - String Interpolation](https://devblogs.microsoft.com/dotnet/string-interpolation-in-csharp-10-and-beyond/)
- Articolo su come prevenire injection attacks:
  [OWASP - Injection Prevention](https://owasp.org/www-community/attacks/Injection_Prevention)
