---
title:                "TypeScript: Szczegółowy opis wysyłania żądania http z uwierzytelnieniem podstawowym"
simple_title:         "Szczegółowy opis wysyłania żądania http z uwierzytelnieniem podstawowym"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Dlaczego

Wysyłanie żądań HTTP z podstawową autoryzacją jest jedną z podstawowych umiejętności w programowaniu TypeScript. Wiele aplikacji internetowych korzysta z tego rodzaju autoryzacji w celu zabezpieczenia dostępu do danych. Dowiedz się dlaczego warto poznać ten temat.

# Jak to zrobić

### Przygotowanie

Przed rozpoczęciem kodowania, musimy upewnić się że nasz projekt jest gotowy do wysyłania żądań HTTP. W tym celu musimy zainstalować paczkę `node-fetch` przy użyciu komendy `npm install node-fetch`.
Następnie w pliku TypeScript musimy dodać import `import fetch from "node-fetch"`.

### Przykład kodu

```TypeScript
fetch('https://example.com', { 
    method: 'GET', 
    headers: {
        Authorization: 'Basic YWRtaW46cGFzc3dvcmQ=' // base64-encoded username:password
    }
})
    .then(res => res.json())
    .then(data => console.log(data));
```
Zobaczmy teraz jak wygląda odpowiedź serwera zwrócona za pomocą `console.log()`:
```TypeScript
{ 
    data: 'Dzięki temu poradnikowi nauczysz się wysyłać żądania HTTP z autoryzacją!'
}
```

# Głębsza analiza

Podstawową autoryzację w sieci można wykorzystać, aby zabezpieczyć dostęp do danych lub zasobów przed niepowołanymi użytkownikami. W tym celu należy przesłać w nagłówku `Authorization` login i hasło zaszyfrowane w formacie base64.

Pamiętaj, że kodowanie base64 nie jest metodą bezpieczną i nie powinno być używane w celach autoryzacyjnych. W przypadku aplikacji produkcyjnej należy wykorzystać protokół HTTPS oraz bardziej zaawansowane metody autoryzacji.

# Zobacz także

- [Dokumentacja Node.js o funkcji Fetch](https://nodejs.org/api/fetch.html)
- [Poradnik o autoryzacji w sieci](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)