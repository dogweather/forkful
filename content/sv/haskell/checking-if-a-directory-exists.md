---
title:    "Haskell: Kontrollera om en katalog existerar"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar kan vara en viktig del av att skriva program på Haskell. Det kan hjälpa till att säkerställa att vår kod fungerar korrekt, oavsett om mappen finns där vi förväntar oss eller inte.

## Så här gör du 

För att utföra en kontroll av en mapp i Haskell, behöver vi använda funktionen `doesDirectoryExist` från `System.Directory` modulen. Vi kan använda den här funktionen för att testa om en mapp existerar eller inte genom att ange sökvägen till mappen som en sträng till funktionen.

```Haskell
import System.Directory

main = do
    let path = "exempel/mapp"

    doesExist <- doesDirectoryExist path
    if doesExist
        then putStrLn "Mappen existerar."
        else putStrLn "Mappen existerar inte."
```

I det här exemplet tilldelar vi värdet av `doesDirectoryExist` till en variabel `doesExist` och använder sedan en `if` sats för att skriva ut ett meddelande beroende på om mappen existerar eller inte.

Om vi till exempel kör detta program och ger sökvägen "exempel/mapp", som är en mapp som finns i samma katalog som vårt program, som indata, kommer följande utdata att skrivas ut:

```
Mappen existerar.
```

Om vi istället ger sökvägen "exempel/mapp2", en mapp som inte finns i samma katalog, som indata, kommer följande utdata att skrivas ut:

```
Mappen existerar inte.
```

## Förklaring

För att vara mer specifik, returnerar `doesDirectoryExist` en `IO Bool` vilket innebär att den utför en I/O-operation och returnerar en `Bool` som kasutom. Den använder `tryIOError` för att fånga eventuella fel som kan uppstå när funktionen försöker öppna mappen. Om det inte finns några fel, returneras `True` och om det finns något fel, returneras `False` och ett undantag genereras.

När vi använder `doesDirectoryExist`, är det viktigt att komma ihåg att sökvägen som vi ger till funktionen inte behöver vara absolut. Om sökvägen är relativ, kommer den att utvärderas i förhållande till det nuvarande arbetsdokumentet.

## Se även

- [System.Directory modulen](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Fördjupning av I/O-operationer i Haskell](https://wiki.haskell.org/Introduction#I.2FO_and_Monads)
- [Flera exempel på directoryExists i praktiken](https://wiki.haskell.org/File_manipulation#Create_a_directory_if_it_doesn.27t_exist)