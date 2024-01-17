---
title:                "Wysyłanie żądania http"
html_title:           "Gleam: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie zapytania HTTP to podstawowa czynność w programowaniu. Polega ona na wysłaniu zapytania do serwera przez program i otrzymaniu odpowiedzi. Programiści wykonują tę czynność w celu pobrania informacji z zewnętrznych źródeł lub komunikacji z innymi programami.

## Jak to zrobić:
```Gleam

import gleam/http

let res = http.send("https://www.example.com")
// Tutaj zmienna 'res' będzie zawierać odpowiedź serwera

```

## W toku:
Wysyłanie zapytania HTTP było możliwe dzięki rozwojowi protokołu HTTP w latach 90. Alternatywami dla tej metody komunikacji są między innymi protokoły FTP czy SSH. W Gleam istnieje moduł "http" umożliwiający wysyłanie zapytań i odbieranie odpowiedzi. Implementacja tego modułu jest oparta na języku Erlang, co zapewnia szybkość i niezawodność.

## Zobacz także:
- Oficjalna dokumentacja Gleam: https://gleam.run/
- Moduł "http" Gleam: https://gleam.run/modules/gleam/http/latest/
- Wprowadzenie do programowania w Gleam: https://medium.com/@lpil/introduction-to-gleam-f818a1f489e0