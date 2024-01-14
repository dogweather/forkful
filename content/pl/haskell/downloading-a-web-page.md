---
title:                "Haskell: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub chcesz nauczyć się programowania, pobieranie stron internetowych może być bardzo ciekawym zadaniem do wykonania. Może pomóc Ci zrozumieć działanie sieci oraz nauczyć Cię podstawowych technik programowania.

## Jak to zrobić

Aby pobrać stronę internetową w Haskell, możesz użyć biblioteki `http-conduit`. Najpierw musisz zainstalować bibliotekę przy użyciu narzędzia `cabal` lub `stack`. Następnie, używając funkcji `simpleHttp`, możesz pobrać zawartość strony i przetworzyć ją w odpowiedni sposób. Przykładowy kod może wyglądać następująco:

```Haskell
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  response <- simpleHttp "https://www.example.com"
  B.writeFile "example.html" response
```
W powyższym przykładzie, pobieramy zawartość strony internetowej pod adresem `https://www.example.com` i zapisujemy ją do pliku `example.html` przy użyciu funkcji `B.writeFile`. Jednakże, może okazać się, że musimy również przetworzyć tę zawartość przed zapisaniem do pliku. W takim przypadku, możemy użyć funkcji `parseUrl` do utworzenia typu danych `Request` i przeprowadzić dodatkowe operacje na pobranej zawartości.

## Wchodząc w szczegóły

Pobieranie stron internetowych w Haskell może być trochę bardziej skomplikowane niż w innych językach programowania. Ponieważ polega na operacjach wejścia-wyjścia, musimy pamiętać o podaniu typu danych `IO` przy użyciu odpowiednich funkcji. Ponadto, funkcja `simpleHttp` może zwrócić wyjątek `HttpException`, który musi być obsłużony przy pomocy konstrukcji `try...catch`. 

Jeśli chcesz dowiedzieć się więcej o pobieraniu stron internetowych w Haskell, warto przeczytać dokumentację biblioteki `http-conduit` oraz poszukać innych przykładów implementacji tego zadania.

## Zobacz również

- [Dokumentacja biblioteki `http-conduit` (po angielsku)](https://www.stackage.org/package/http-conduit)
- [Przykładowa implementacja pobierania stron internetowych w Haskell (po angielsku)](https://www.schoolofhaskell.com/school/advanced-haskell/parsing-websites-and-rendering-html)