---
title:                "Pobieranie strony internetowej"
html_title:           "Haskell: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie strony internetowej może być użyteczne w wielu sytuacjach, na przykład przy tworzeniu skanera sieci, pobieraniu danych z serwisów internetowych lub prostym sprawdzeniu zawartości strony.

## Jak to zrobić

Pobieranie strony internetowej w Haskell jest proste, dzięki zastosowaniu biblioteki "http-conduit". Najpierw musimy zainstalować tę bibliotekę, korzystając z menedżera pakietów Cabal:

```
cabal install http-conduit
```

Następnie, importujemy ją w naszym programie:

```
import Network.HTTP.Conduit
```

Teraz możemy wykorzystać funkcję "simpleHttp" z biblioteki "http-conduit", aby pobrać zawartość strony internetowej. Poniższy kod pobierze zawartość strony "www.google.com" i wyświetli ją na ekranie:

```
main = do
  content <- simpleHttp "http://www.google.com"
  putStrLn $ "Zawartość strony: " ++ show content
```

Po uruchomieniu programu, powinniśmy zobaczyć na ekranie zawartość strony "www.google.com". Zamiast wyświetlać zawartość na ekranie, możemy też zapisać ją do pliku, korzystając z funkcji "writeFile":

```
writeFile "google.html" content
```

Ta sama funkcja może być wykorzystana do pobierania danych z innych serwisów internetowych lub stron z danymi w formacie JSON. Wówczas musimy wykorzystać funkcję "requestBody" zamiast "simpleHttp", aby pobrać odpowiedź w formacie odpowiednim dla naszego serwisu.

## Wgląd w temat

Pobieranie stron internetowych może być trudne w niektórych przypadkach, gdyż niektóre strony wykorzystują zabezpieczenia, takie jak CAPTCHA, które utrudniają automatyczne pobieranie danych. W takich przypadkach, musimy użyć dodatkowych narzędzi lub bibliotek, takich jak "selenium", aby symulować zachowanie przeglądarki internetowej i ominąć zabezpieczenia.

## Zobacz też

- [Dokumentacja biblioteki "http-conduit"](https://hackage.haskell.org/package/http-conduit)
- [Przykładowe programy pobierające dane z sieci w Haskellu](https://wiki.haskell.org/Examples/Networking)