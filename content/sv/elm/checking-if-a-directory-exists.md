---
title:    "Elm: Kontrollera om en katalog finns"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en katalog finns är en viktig uppgift för Elm programmerare. Det tillåter oss att skapa mer robusta program som kan hantera eventuella fel eller oförutsedda situationer.

## Hur man gör det

För att kontrollera om en katalog finns i Elm, kan vi använda funktionen `Directory.doesExist` från modulen `Elm.System`, som returnerar en `Result` typ som antingen är `Err` eller `Ok`. Om katalogen finns kommer `Result` att vara `Ok`, annars kommer det att vara `Err`.

```Elm
import Elm.System

checkDirectory : String -> Cmd msg
checkDirectory directory =
    case Elm.System.doesExist directory of
        Ok ->
            -- Katalogen finns och vi kan fortsätta med våra åtgärder
        Err ->
            -- Katalogen finns inte och vi behöver hantera detta

```

## Djupdykning

Vad händer om vi bara kollar efter en katalognamn som inte finns? Detta kan leda till att vårt program kraschar eller att vi inte kan hantera problemet på ett lämpligt sätt. För att undvika detta, kan vi använda funktionen `Directory.existsWithParents` som även kontrollerar om eventuella överordnade kataloger existerar.

```Elm
import Elm.System

checkDirectory : String -> Cmd msg
checkDirectory directory =
    case Elm.System.existsWithParents directory of
        Ok ->
            -- Katalogen (och eventuella överordnade) finns och vi kan fortsätta med våra åtgärder
        Err ->
            -- Katalogen (eller någon överordnad katalog) finns inte och vi behöver hantera detta

```

Det är också viktigt att notera att funktionerna `doesExist` och `existsWithParents` bara kontrollerar om en specifik katalog finns, och inte andra objekt som kan ha samma namn (t.ex. filer).

## Se även

- [Elm.System modul](https://package.elm-lang.org/packages/elm/core/latest/Elm-System)
- [Resolving Errors in Elm](https://guide.elm-lang.org/error_handling/)
- [Elm Community Packages](https://package.elm-lang.org/)