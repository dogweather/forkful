---
title:                "Wysyłanie żądania http z podstawową autoryzacją"
html_title:           "Go: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Co to jest i dlaczego to robimy?

Wysłanie żądania HTTP z podstawową autoryzacją to proces, w którym wysyłamy żądanie do serwera, ale do uzyskania dostępu wymagana jest autoryzacja za pomocą podstawowych danych. Programiści robią to po to, aby zabezpieczyć i ograniczyć dostęp do zasobów chronionych na serwerze.

Jak to zrobić:

```Go
url := "https://example.com/api/endpoint"
    
req, err := http.NewRequest("GET", url, nil)
    
username := "admin"
password := "pass123"
    
req.SetBasicAuth(username, password)

res, err := http.DefaultClient.Do(req)
    
defer res.Body.Close()
    
if res.StatusCode == 200 {
    fmt.Println("Udane żądanie HTTP z podstawową autoryzacją!")
} else {
    fmt.Println("Autoryzacja nie powiodła się.")
}
```

## Deep Dive

Kontekst historyczny:

Autoryzacja została zaprojektowana dla protokołu HTTP przez IETF (Internet Engineering Task Force) w 1996 roku. Jest to jedna z najprostszych i najpowszechniejszych metod autoryzacji w sieciach komputerowych.

Alternatywy:

Istnieje kilka innych metod autoryzacji, takich jak autoryzacja HMAC, autoryzacja tokenów dla dostępu, autoryzacja oparta na sesjach i inne. Każda z nich ma swoje własne zalety i wady.

Szczegóły implementacji:

Wysyłanie żądania HTTP z podstawową autoryzacją wymaga ustawienia nagłówka "Authorization" z odpowiednim kodowaniem danych autoryzacyjnych. W przykładzie kodu powyżej, użyliśmy funkcji ```SetBasicAuth``` , która wygenerowała i ustawiła ten nagłówek automatycznie. Należy jednak pamiętać, że w rzeczywistości, należy dokonać wielu innych działań, takich jak sprawdzenie poprawności danych autoryzacyjnych, obsługa błędów, itp.

## Zobacz też:

- Oficjalna dokumentacja Go dla pakietu "net/http": https://golang.org/pkg/net/http/
- Dokumentacja IETF dotycząca autoryzacji HTTP: https://tools.ietf.org/html/rfc7615