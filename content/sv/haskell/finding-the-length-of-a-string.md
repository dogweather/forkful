---
title:    "Haskell: Att hitta längden på en sträng"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Varför

Att hitta längden på en sträng är en vanlig uppgift när man programmerar. Det kan vara användbart när man vill göra vissa beräkningar eller när man behöver veta hur många tecken en användare har skrivit in.

# Hur man gör det

Att hitta längden på en sträng är enkelt med hjälp av Haskell. Nedan följer ett enkelt exempel på hur man gör det:

```Haskell
-- Skapa en funktion som tar emot en sträng och returnerar längden
längd :: String -> Int
längd sträng = length sträng  -- Använd den inbyggda funktionen "length"

-- Använd funktionen för att hitta längden på en sträng
main = do
  let sträng = "Hej, världen!"
  putStrLn $ "Längden på strängen \"" ++ sträng ++ "\" är " ++ show (längd sträng) ++ " tecken."
```

Output:
```
Längden på strängen "Hej, världen!" är 13 tecken.
```

# Djupdykning

När vi använder funktionen "length" för att hitta längden på en sträng kommer den att räkna antalet tecken inklusive mellanslag och specialtecken. Om man endast vill räkna bokstäverna i en sträng kan man använda sig av funktionen "filter". Här är ett exempel på hur man kan göra det:

```Haskell
-- Skapa en funktion som endast tar med bokstäver i längden
bokstäver :: String -> Int
bokstäver sträng = length $ filter (\x -> isLetter x) sträng  -- Använd "filter" och "isLetter" från Data.Char för att hitta bokstäverna

-- Använd funktionen för att hitta antalet bokstäver i en sträng
main = do
  let sträng = "Hello, world!"
  putStrLn $ "Antalet bokstäver i strängen \"" ++ sträng ++ "\" är " ++ show (bokstäver sträng) ++ "."
```

Output:
```
Antalet bokstäver i strängen "Hello, world!" är 10.
```

# Se även

- [Haskell.org](https://www.haskell.org)
- [Data.Char-modulens dokumentation](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html)
- [Exempel på funktioner i Haskell](https://wiki.haskell.org/Function)