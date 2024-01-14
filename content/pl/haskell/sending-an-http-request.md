---
title:                "Haskell: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

Cześć! W dzisiejszych czasach, przesyłanie zapytań HTTP jest nieodłączną częścią niemal każdej aplikacji lub strony internetowej. W tym krótkim wpisie, dowiesz się, dlaczego jest tak ważne, jak to zrobić oraz jak działa w głębi.

## Dlaczego

Wysyłanie zapytań HTTP jest niezbędnym narzędziem w dzisiejszym świecie internetu. Dzięki nim, aplikacje mogą komunikować się ze sobą, wymieniać informacje oraz pobierać zasoby z serwera. Bez tego, większość stron internetowych nie działałaby poprawnie.

## Jak to zrobić

W Haskellu, wysyłanie zapytań HTTP jest możliwe dzięki bibliotece `http-conduit`. Możemy to zrobić w prosty sposób, używając funkcji `httpLBS`. W poniższym przykładzie, wyślemy zapytanie GET do strony Stack Overflow i wyświetlimy odpowiedź w konsoli.

```Haskell
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L

main = do
  response <- httpLBS "https://stackoverflow.com"
  print $ responseBody response
```

### Wyjście:

```HTML
<!doctype html>
<html itemscope="" itemtype="http://schema.org/QAPage" lang="pl">
  <head>
    <title>Stack Overflow</title>
    ...
  </head>
  <body class="home-page unified-theme">
    ...
  </body>
</html>
```

W powyższym przykładzie, użyliśmy `qualified` dołączenia modułu `Data.ByteString.Lazy` aby uniknąć konfliktów nazw z innymi modułami. Po wykonaniu zapytania, otrzymaliśmy cały kod HTML strony w postaci `ByteString` i wydrukowaliśmy go do konsoli.

## Deep Dive

Wysyłanie zapytań HTTP może wydawać się proste, ale istnieje wiele szczegółów, które warto poznać. Na przykład, możemy dostosować nasze zapytanie dodając nagłówki lub zmieniając metodę zapytania.

```Haskell
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L

main = do
  request <- parseUrlThrow "https://postman-echo.com/post"
  let request' = setRequestMethod "POST" $ setRequestHeader "Content-Type" ["application/json"] request
  response <- httpLBS request'
  print $ responseBody response
```

### Wyjście:

```JSON
{"args":{},"data":"","files":{},"form":{},"headers":{"x-forwarded-proto":"https","x-forwarded-port":"443","host":"postman-echo.com","content-type":"application/json","content-length":"0","accept-encoding":"gzip"},"json":null,"url":"https://postman-echo.com/post"}
```

W powyższym przykładzie, zmieniliśmy metodę zapytania na `POST` oraz dodaliśmy nagłówek `"Content-Type"` z wartością `"application/json"`. Teraz, możemy wysyłać zapytania z danymi JSON i otrzymywać odpowiedzi w formacie JSON.

## Zobacz również

- [Dokumentacja biblioteki `http-conduit`](https://hackage.haskell.org/package/http-client)
- [Tutorial o wysyłaniu zapytań HTTP w Haskellu](https://www.stackbuilders.com/tutorials/haskell/http-requests/)
- [Blog o rozwoju aplikacji webowych w Haskellu](https://www.haskell.org/wiki/Haskell_web_programming)