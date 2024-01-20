---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i Dlaczego? 

Pobieranie strony internetowej oznacza po prostu ściąganie jej kodu HTML na twój komputer. Programiści robią to, aby analizować ten kod, przetwarzać dane lub monitorować zmiany na stronie. 

## Jak to zrobić:

Poniżej znajduje się przykładowy kod w Haskellu, który pobiera stronę internetową.

```Haskell
import Network.HTTP 
import Network.URI (parseURI)

getURLContent :: String -> IO (Either String String)
getURLContent url = do
    case parseURI url of
        Nothing -> return $ Left ("Invalid URL: " ++ url)
        Just uri -> do
            body <- simpleHTTP (mkRequest GET uri) >>= getResponseBody
            return $ Right body
```

Uruchomienie tego kodu dla URL-a, na przykład "http://www.google.com", zwróci kod HTML strony:

```Haskell
main = do
    content <- getURLContent "http://www.google.com"
    case content of
        Left errorMsg -> print errorMsg
        Right body -> putStrLn body
```

## Bardziej szczegółowo:

Pobieranie stron internetowych ma długą historię, idącą z powrotem do czasów, kiedy internet był nadal w powijakach. Haskelem można to zrobić na wiele różnych sposobów - używając różnych bibliotek, takich jak `http-conduit`, `wreq` czy `http-client`.

Istotne jest, abyś przestrzegał zasad etycznych podczas korzystania z tych technik, przestrzegając zasady `robots.txt` na stronach internetowych i nie przeciążając serwerów.

Szczegóły implementacji `simpleHTTP` i `getResponseBody` zależą od wielu czynników, takich jak to, czy serwer obsługuje HTTP/2, czy strona jest zaszyfrowana, itp.

## Zobacz również:

1. Dokumentacja biblioteki Network.HTTP: <http://hackage.haskell.org/package/HTTP-4000.3.12/docs/Network-HTTP.html>.
2. Inne biblioteki do pobierania stron w Haskellu: `http-conduit` (<https://hackage.haskell.org/package/http-conduit-2.3.7.3>), `wreq` (<https://hackage.haskell.org/package/wreq-0.5.3.2>), `http-client` (<https://hackage.haskell.org/package/http-client-0.6.4.1>).
3. Zasady dla robotów internetowych: <https://pl.wikipedia.org/wiki/Robots.txt>.
4. Więcej o historii Internetu: <https://pl.wikipedia.org/wiki/Historia_Internetu>.