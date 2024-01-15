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

## Dlaczego

Podstawowa autoryzacja jest jedną z najczęściej stosowanych metod uwierzytelniania w protokole HTTP. Wysyłając zapytanie HTTP z podstawowym uwierzytelnieniem, użytkownik może dostarczyć nazwę użytkownika i hasło, które będą weryfikowane przez serwer w celu autoryzacji dostępu. Jest to przydatne w przypadku korzystania z aplikacji internetowych, gdzie istnieje potrzeba zabezpieczenia dostępu do określonych zasobów.

## Jak

\`\`\`Go
// Załadowanie potrzebnych pakietów
import (
    "fmt"
    "net/http"
    "encoding/base64"
)

func main() {
    // Ustawienie adresu URL, do którego zostanie wysłane zapytanie
    url := "https://moja-aplikacja.com/zasoby"

    // Ustawienie danych uwierzytelniających
    username := "moj_uzytkownik"
    password := "moje_haslo"

    // Zakodowanie danych uwierzytelniających za pomocą Base64
    auth := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))

    // Ustawienie nagłówka uwierzytelniającego w zapytaniu
    req, _ := http.NewRequest("GET", url, nil)
    req.Header.Set("Authorization", "Basic " + auth)

    // Wysłanie zapytania i obsłużenie potencjalnego błędu
    client := &http.Client{}
    resp, err := client.Do(req)
    if err != nil {
        fmt.Println("Wystąpił błąd:", err)
        return
    }
    defer resp.Body.Close()

    // Wypisanie odpowiedzi serwera
    fmt.Println(resp.Status)
}
\`\`\`

**Output:**

200 OK

## Deep Dive

Podstawowa autoryzacja jest często uważana za nieskuteczną i niebezpieczną ze względu na to, że dane uwierzytelniające są przesyłane w formie niezaszyfrowanej. Dlatego zaleca się stosowanie innych metod uwierzytelniania (np. uwierzytelnianie tokenowe) dla większego bezpieczeństwa. Jednak w niektórych przypadkach, np. w przypadku wewnętrznych aplikacji firmowych, podstawowa autoryzacja może być wystarczająca.

## Zobacz także

- [Dokumentacja Go](https://golang.org/doc/)
- [Oficjalny poradnik HTTP w Go](https://golang.org/pkg/net/http/)
- [Podstawowa autoryzacja w protokole HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#http_basic_authentication)