---
title:                "Pobieranie strony internetowej"
date:                  2024-01-20T17:44:26.387677-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pobieranie strony internetowej"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? 
Czyli po polsku: "Co i dlaczego?"

Pobieranie strony internetowej to proces, w którym uzyskujemy jej dane (najczęściej w formacie HTML) bezpośrednio na nasz komputer. Programiści robią to, aby pracować z treścią strony - analizując, testując, czy integrując systemy.

## How to:
Czyli po polsku: "Jak to zrobić:"

W Haskellu, możemy użyć biblioteki `http-conduit` do pobierania stron internetowych. Oto przykład użycia:

```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://example.com"
    let statusCode = getResponseStatusCode response
    let body = getResponseBody response
    putStrLn $ "Status code: " ++ show statusCode
    putStrLn $ "Response body: " ++ show body
```

Sample output:

```
Status code: 200
Response body: "<!doctype html>..."
```

## Deep Dive
Czyli po polsku: "Wgłębiamy się":

W przeszłości, by pobrać stronę internetową, można było użyć biblioteki `http`, ale `http-conduit` jest obecnie zalecanym rozwiązaniem, oferującym większą wygodę i funkcje, jak automatyczne przechowywanie sesji czy obsługę połączeń https. Za kulisami, `http-conduit` korzysta z potoków (`conduit`), które efektywnie przetwarzają strumienie danych i mogą obsłużyć zarówno małe, jak i duże odpowiedzi.

## See Also
Czyli po polsku: "Zobacz również":

- Oficjalna dokumentacja `http-conduit`: https://hackage.haskell.org/package/http-conduit
- Opis potoków w Haskellu (`conduit`): https://hackage.haskell.org/package/conduit
- Tutorial Haskell-a: http://learnyouahaskell.com/chapters
