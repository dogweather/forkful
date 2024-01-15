---
title:                "Wysyłanie żądania http z uwierzytelnieniem podstawowym"
html_title:           "Fish Shell: Wysyłanie żądania http z uwierzytelnieniem podstawowym"
simple_title:         "Wysyłanie żądania http z uwierzytelnieniem podstawowym"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Często jesteśmy zmuszeni do wysyłania żądań HTTP ze standardowym uwierzytelnieniem, czy to w celu autentykacji lub pobrania danych z API. Wykorzystanie podstawowego uwierzytelnienia jest proste i dostępne we wszystkich językach programowania, w tym także w Fish Shell. W tym artykule dowiesz się, jak szybko i łatwo wysłać żądanie HTTP z podstawowym uwierzytelnieniem w Fish Shell.

## Jak To Zrobić

Aby wysłać żądanie HTTP z podstawowym uwierzytelnieniem, możemy wykorzystać wiersz poleceń cURL. W Fish Shell, możemy wykonać to polecenie używając funkcji `curl`:

```Fish Shell
curl -u username:password https://example.com/api
```

W przypadku, gdy chcemy wykonać bardziej skomplikowane żądanie HTTP, możemy wykorzystać bibliotekę HTTPie. Najpierw musimy zainstalować ją przy użyciu menedżera pakietów `fisher`:

```Fish Shell
fisher install jorgebucaran/fisher
fisher install httpie
```

Następnie, możemy wywołać funkcję `http` i przekazać odpowiednie parametry, w tym uwierzytelnienie, do naszego żądania:

```Fish Shell
http -a username:password https://example.com/api
```

Uwaga: Pamiętaj, żeby zawsze używać bezpiecznego połączenia (HTTPS) przy wysyłaniu poufnych danych z uwierzytelnieniem.

## Deep Dive

Podczas wysyłania żądań HTTP z podstawowym uwierzytelnieniem, nasz login i hasło są przesyłane w nagłówku `Authorization`. Jest to metoda bezpieczna, ponieważ użytkownik, który przechwyci nasze żądanie, nie będzie w stanie odczytać naszego hasła bez zdekodowania kodowania Base64, które jest używane do przekazywania informacji uwierzytelniających. Jednakże, należy pamiętać, że z kodowaniem Base64 jest jedynie sposobem zamaskowania informacji, a nie metoda bezpiecznej transmisji.

W przypadku wykorzystania biblioteki HTTPie do wysyłania żądań HTTP z uwierzytelnieniem, nasze hasło zostanie przechowane w pamięci przez krótki okres czasu. Aby uniknąć tego, możemy użyć opcji `--auth-type=none` lub `--auth-type=digest`, co spowoduje, że nasz login i hasło nie będą przechowywane w pamięci.

## Zobacz Również

- [Dokumentacja cURL](https://curl.haxx.se/)
- [Więcej informacji o HTTPie](https://httpie.org/)
- [Fish Shell - Oficjalna strona](https://fishshell.com/)