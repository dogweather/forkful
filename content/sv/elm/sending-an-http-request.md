---
title:                "Sända en http-begäran"
html_title:           "Elm: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Anledningen till att skicka en HTTP-begäran är för att hämta data från en extern källa, till exempel en API eller en webbsida. Detta är en viktig funktion inom programmering då det innebär att man kan hämta och bearbeta information från olika källor på internet.

## Så här gör du

För att skicka en HTTP-begäran i Elm, behöver vi använda modulen `Http` som är en del av standardbiblioteket. Först måste vi importera denna modul i vårt program, sedan kan vi använda en av funktionerna inom modulen för att skapa och skicka vår begäran.

```Elm
import Http

request : Http.Request String
request =
  Http.get "https://example.com"
```

I detta exempel skapar vi en HTTP GET-begäran till en fiktiv webbadress `https://example.com` och sparar den i en variabel. Därefter behöver vi skicka begäran genom att använda funktionen `Http.send` och ange vår begäran som ett argument. Vi måste också definiera en modul som ska hantera svaret från begäran.

```Elm
type Msg
  = FetchSuccess (Result Http.Error String)
  | FetchError Http.Error

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    FetchSuccess result ->
      case result of
        Ok data ->
          -- Handle successful response
          ( model, Cmd.none )
        
        Err error ->
          -- Handle error response
          ( model, Cmd.none )
    
    FetchError error ->
      -- Handle error message
      ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Http.send update request
```

Vi kan se att vår `update`-funktion har en `Msg` som hanterar both framgångsrika och misslyckade svaren från vår begäran. Beroende på resultatet kan vi hantera datan eller hantera eventuella fel som uppstår. Slutligen måste vi också inkludera en `subscriptions`-modul som skickar vår begäran och väntar på ett svar.

## Djupdykning

Det finns många olika funktioner och metoder som kan användas vid skickande av HTTP-begäran i Elm. Till exempel kan vi använda funktionen `Http.post` för att skicka en POST-begäran istället för en GET och skicka med data i begäran som argument. Vi kan också använda `Http.sendBinary` för att skicka en binär data i vår begäran som en Blob, fil eller ArrayBuffer.

## Se även

- [Elm API Dokumentation för `Http` modulen](https://package.elm-lang.org/packages/elm/http/latest/Http)
- [Elm Guide för webbprogrammering](https://guide.elm-lang.org/webapps/)