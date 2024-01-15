---
title:                "Pobieranie strony internetowej"
html_title:           "Go: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub tylko zaczynasz przygodę z programowaniem, możliwe że zainteresuję Cię fakt, że Go jest językiem bardzo popularnym i cały czas zyskuje na popularności. Dzięki swojej prostocie i wydajności, jest idealnym językiem do tworzenia aplikacji webowych. Pobieranie stron internetowych jest jedną z często wykorzystywanych funkcji w tworzeniu aplikacji internetowych, dlatego w tym artykule pokażę Ci jak można to zrobić w Go.

## Jak To Zrobić

### Pobieranie strony internetowej

W celu pobrania strony internetowej w Go, musimy użyć pakietu `net/http`, który zapewnia funkcje HTTP wraz z obsługą protokołu TLS. Następnie wykorzystujemy funkcję `Get()` z tego pakietu, aby pobrać stronę internetową w formie obiektu typu `Response`.

```Go
resp, err := http.Get("https://example.com")
```

### Odczytywanie odpowiedzi

Przy użyciu obiektu `Response` możemy teraz odczytać odpowiedź i przypisać ją do zmiennej. W tym przykładzie wyświetlimy status odpowiedzi oraz treść strony internetowej w konsoli.

```Go
fmt.Println("Status:", resp.Status)
defer resp.Body.Close()
body, err := ioutil.ReadAll(resp.Body)
if err != nil {
    log.Fatal(err)
}
fmt.Println(string(body))
```

### Zapisywanie odpowiedzi do pliku

Jeśli chcesz zapisać pobraną stronę internetową do pliku, możesz to zrobić przy użyciu funkcji `WriteFile()` z pakietu `ioutil`.

```Go
err := ioutil.WriteFile("example.html", body, 0644)
if err != nil {
    log.Fatal(err)
}
```

## Dogłębne Omówienie

W powyższych przykładach użyliśmy prostych funkcji, aby pobrać i przetworzyć stronę internetową w Go. Jednak pakiet `net/http` oferuje również wiele innych funkcji, takich jak obsługa ciasteczek czy przekazywanie nagłówków w żądaniach. Jeśli potrzebujesz bardziej zaawansowanych funkcji, zawsze możesz zapoznać się z dokumentacją tego pakietu.

## Zobacz także

- [Dokumentacja pakietu `net/http`](https://golang.org/pkg/net/http/)
- [Aplikacje webowe w Go - szybki start](https://www.golangprograms.com/go-program-to-fetch-a-webpage-from-the-given-url.html)
- [Simple GET request tutorial (YouTube)](https://www.youtube.com/watch?v=YK-n9McB1Pk)