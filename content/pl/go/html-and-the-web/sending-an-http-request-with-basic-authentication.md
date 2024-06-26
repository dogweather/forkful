---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:21.231843-07:00
description: "Jak to zrobi\u0107: Aby wys\u0142a\u0107 \u017C\u0105danie HTTP z podstawow\u0105\
  \ autoryzacj\u0105 w Go, musisz przygotowa\u0107 nag\u0142\xF3wki \u017C\u0105dania,\
  \ aby zawiera\u0142y pole `Authorization`, wype\u0142nione\u2026"
lastmod: '2024-03-13T22:44:34.850478-06:00'
model: gpt-4-0125-preview
summary: "Aby wys\u0142a\u0107 \u017C\u0105danie HTTP z podstawow\u0105 autoryzacj\u0105\
  \ w Go, musisz przygotowa\u0107 nag\u0142\xF3wki \u017C\u0105dania, aby zawiera\u0142\
  y pole `Authorization`, wype\u0142nione twoimi po\u015Bwiadczeniami we w\u0142a\u015B\
  ciwym formacie."
title: "Wysy\u0142anie \u017C\u0105dania HTTP z podstawowym uwierzytelnianiem"
weight: 45
---

## Jak to zrobić:
Aby wysłać żądanie HTTP z podstawową autoryzacją w Go, musisz przygotować nagłówki żądania, aby zawierały pole `Authorization`, wypełnione twoimi poświadczeniami we właściwym formacie. Poniżej znajduje się przykład, który demonstruje, jak wykonać żądanie GET do punktu końcowego API wymagającego podstawowej autoryzacji:

```go
package main

import (
	"fmt"
	"net/http"
	"encoding/base64"
)

func main() {
	client := &http.Client{}
	req, err := http.NewRequest("GET", "http://example.com/api/data", nil)
	if err != nil {
		panic(err)
	}

	username := "yourUsername"
	password := "yourPassword"
    // Kodowanie poświadczeń
	auth := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))
    // Ustawianie nagłówka Authorization
	req.Header.Add("Authorization", "Basic " + auth)

	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	fmt.Println("Status odpowiedzi:", resp.Status)
}
```

Uruchomienie tego kodu wyśle żądanie GET pod podany adres URL z niezbędnym nagłówkiem autoryzacji. Wynik będzie wyglądał mniej więcej tak, w zależności od twojego punktu końcowego i usługi:

```
Status odpowiedzi: 200 OK
```

## Szczegółowe omówienie
Podstawowa autoryzacja w żądaniach HTTP jest powszechnie wspieraną metodą egzekwowania kontroli dostępu do zasobów sieciowych. Polega po prostu na wysyłaniu nazwy użytkownika i hasła przy każdym żądaniu, co ułatwia implementację, ale nie jest najbezpieczniejszą dostępną metodą. Główną wadą jest to, że jeśli nie jest używana w połączeniu z SSL/TLS, poświadczenia są wysyłane w postaci jawnej (ponieważ Base64 łatwo odkodować). Może to potencjalnie narazić wrażliwe informacje na ataki typu man-in-the-middle.

W Go, wysyłanie tych żądań polega na bezpośredniej manipulacji nagłówkiem `Authorization`. Chociaż standardowa biblioteka Go (`net/http`) dostarcza potężne prymitywy do obsługi komunikacji HTTP(s), jest ona stosunkowo niskopoziomowa, wymagając od programistów ręcznego obsługiwania różnych aspektów żądania/odpowiedzi HTTP. Daje to programistom dużą elastyczność, ale oznacza również, że należy zwrócić większą uwagę na implikacje dotyczące bezpieczeństwa, kodowania i poprawnego zarządzania nagłówkami.

Dla aplikacji wymagających wyższego poziomu bezpieczeństwa, należy rozważyć bardziej zaawansowane systemy autentykacji, takie jak OAuth2 czy JWT (JSON Web Tokens). Te podejścia zapewniają bardziej solidne funkcje bezpieczeństwa i są szeroko wspierane w nowoczesnych API i usługach. Rozszerzający się ekosystem Go obejmuje liczne biblioteki i narzędzia (takie jak `golang.org/x/oauth2`, wśród innych) ułatwiające implementację tych bardziej bezpiecznych metod autoryzacji, co ułatwia programistom wdrażanie bezpiecznych, skutecznych i nowoczesnych mechanizmów autoryzacji w swoich aplikacjach.
