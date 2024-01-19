---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem oznacza dodanie nagłówka `Authorization: Basic {base64encode('username:password')}` do żądań HTTP, aby zweryfikować tożsamość użytkownika. Programiści robią to, aby uzyskać dostęp do chronionych zasobów.

## Jak to zrobić:

W Go wystarczy kilka linii kodu:

```Go
package main

import (
	"net/http"
	"encoding/base64"
)

func main() {
	client := &http.Client{}
	req, _ := http.NewRequest("GET", "http://example.com", nil)
	basicAuth := "Basic " + base64.StdEncoding.EncodeToString([]byte("username:password"))
	req.Header.Add("Authorization", basicAuth)
	resp, _ := client.Do(req)
	// obsłuż odpowiedź ...
}
```

## Głębsze Zanurzenie:

(1) Historia: Podstawowe uwierzytelnianie to najstarszy sposób uwierzytelniania dla HTTP. Został zdefiniowany w oryginalnej specyfikacji HTTP/1.0 w 1996 roku.

(2) Alternatywy: Pozniejsze metody uwierzytelniania takie jak OAuth, Token JWT oraz API Key mają lepsze funkcje bezpieczeństwa, ale są bardziej skomplikowane do implementacji.

(3) Szczegóły implementacji: W `http.NewRequest`, jesteśmy zobligowani do kodowania `username:password` w Base64. Następnie, dodajemy to do nagłówka `Authorization`. Prośba jest wysyłana za pośrednictwem `client.Do(req)`.

## Zobacz też:

(1) Specyfikacja uwierzytelniania HTTP Basic: https://tools.ietf.org/html/rfc7617

(2) Dokumentacja dla pakietu `http` w Golang: https://golang.org/pkg/net/http/

(3) Więcej informacji o innych metodach uwierzytelniania: https://developer.okta.com/books/api-security/authn/api-authentication-options/