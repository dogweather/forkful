---
title:                "Wysyłanie zapytania http z podstawową autoryzacją"
date:                  2024-01-20T18:01:49.501823-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie zapytania http z podstawową autoryzacją"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wysyłanie żądania HTTP z podstawową autentykacją to proces przesyłu nazwy użytkownika i hasła w celu uzyskania dostępu do zabezpieczonych zasobów. Programiści używają tego do komunikacji z API, które wymagają uwierzytelnienia, pozwalając na bezpieczną interakcję z danymi.

## Jak to zrobić:
```Go
package main

import (
	"bytes"
	"encoding/base64"
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	client := &http.Client{}

	req, err := http.NewRequest("GET", "http://twojserwer.com/dane", nil)
	if err != nil {
		// obsługa błędu
		panic(err)
	}

	username := "twoj_uzytkownik"
	password := "twoje_haslo"
	encodedCredentials := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))
	req.Header.Add("Authorization", "Basic "+encodedCredentials)

	resp, err := client.Do(req)
	if err != nil {
		// obsługa błędu
		panic(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		// obsługa błędu
		panic(err)
	}
	fmt.Println(string(body))
}
```
Po uruchomieniu kodu zobaczysz w konsoli wynik żądania HTTP.

## Deep Dive
Autentykacja podstawowa to jedna z najstarszych metod uwierzytelniania w HTTP. Nie jest najbezpieczniejsza, ponieważ dane są tylko enkodowane, a nie szyfrowane - mogą być łatwo przechwycone. Alternatywami są między innymi OAuth, tokeny API, czy połączenia HTTPS. W Go, do żądania HTTP z autentykacją możemy użyć gotowych pakietów, takich jak `http.Client`, który pozwala na konfigurację nagłówków. 

## See Also
- Dokumentacja Go `http` pakietu: https://pkg.go.dev/net/http
- Specyfikacja HTTP Basic Access Authentication: https://tools.ietf.org/html/rfc7617
- Tutorial dla autentykacji OAuth w Go: https://oauth.net/2/grant-types/password/
