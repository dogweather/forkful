---
title:                "Bash: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Z pewnością wielu z Was słyszało już o tajemniczych "HTTP requestach", ale co to takiego i po co używać ich w programowaniu? Dzięki nim możemy wysyłać i otrzymywać dane z serwera, a to jest szczególnie ważne w przypadku tworzenia aplikacji internetowych.

## Jak To Zrobić

Aby wysłać HTTP request, należy użyć polecenia "curl" w terminalu Bash. W poniższym przykładzie wyślemy GET request do serwera Google i zobaczymy odpowiedź w terminalu.

```Bash
curl www.google.com
```

Powinno pojawić się kilka linii tekstu z odpowiedzią od serwera Google. Możemy również dodać parametry do naszego requestu, na przykład, aby przekazać dane formularza w formacie JSON lub wysłać żądanie POST. Poniższy przykład pokazuje, jak wysłać POST request z danymi w formacie JSON.

```Bash
curl -X POST -H "Content-Type: application/json" -d '{"name": "John", "age": 30}' www.example.com
```

Jeśli chcemy zobaczyć tylko nagłówki odpowiedzi, możemy użyć flagi "-I", a jeśli chcemy zobaczyć szczegółową informację o requestach i odpowiedziach, możemy użyć flagi "-v".

```Bash
curl -I www.google.com
curl -v www.example.com
```

Warto również wspomnieć o tym, że polecenie "curl" jest bardzo elastyczne i posiada wiele innych opcji, które mogą być przydatne w różnych sytuacjach. Możesz je poznać, używając flagi "--help" lub przeglądając dokumentację.

## Głębszy Zanurzenie

HTTP requesty są często używane w programowaniu, szczególnie w przypadku tworzenia aplikacji internetowych. Pozwalają nam one na komunikację z serwerem i możliwość przesyłania oraz odbierania danych. W dzisiejszych czasach, gdy wiele aplikacji działa w oparciu o usługi internetowe, umiejętność tworzenia i obsługi HTTP requestów jest niezwykle przydatna dla programistów.

Jednym z ciekawszych zastosowań HTTP requestów jest tworzenie automatycznych skryptów, które pobierają i przetwarzają dane z różnych źródeł. Dzięki temu możemy na przykład zautomatyzować proces pobierania danych do naszej bazy danych lub regularne aktualizacje naszych zasobów zewnętrznych.

Oprócz tego, istnieją różne metody obsługi HTTP requestów w różnych językach programowania, dlatego warto poszerzyć swoją wiedzę w tym temacie, aby być przygotowanym na różne przypadki.

## Zobacz Również

- [HTTP Request w dokumentacji curl](https://curl.haxx.se/docs/httpscripting.html)
- [Poradnik o HTTP requestach na blogu Girl vs Code](http://girlvscode.com/http-request-basics-python/)

Dziękujemy za przeczytanie naszego wpisu na blogu o programowaniu w Bash dotyczącego HTTP requestów. Mamy nadzieję, że teraz masz lepsze pojęcie o tym, jak używać tego narzędzia w swoich projektach. A jeśli chcesz dowiedzieć się więcej o programowaniu w Bash, zachęcamy do przeglądania innych naszych wpisów. Do zobaczenia!