---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

---
## Co i Dlaczego?
Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem to metoda zabezpieczania transmisji cyfrowych. Programiści korzystają z niej, aby kontrolować dostęp do zasobów sieciowych, gwarantując jednocześnie prywatność danych.

## Jak to zrobić:
Aby wysłać żądanie HTTP z podstawowym uwierzytelnianiem, musimy dodać nagłówek autorów `Authorization` do naszego żądania. Wykorzystajmy popularny pakiet Alamofire do wykonania żądania HTTP.

```Swift
import Alamofire

let user = "username"
let password = "password"

let credential = URLCredential(user: user, password: password, persistence: .forSession)

AF.request("https://api.website.com/auth", method: .get)
    .authenticate(with: credential)
    .response { response in
        debugPrint(response)
}
```

Po uruchomieniu powyższego kodu, otrzymamy odpowiedź serwera w konsoli debugowania.

## Dogłębnie:
Historia uwierzytelniania w internecie jest długa i skomplikowana. Uwierzytelnianie Basic jest jednym z najstarszych, po raz pierwszy określonym w RFC 1945 w 1996 roku. Mimo licznych alternatyw, wciąż jest szeroko stosowany, ponieważ jest prosty i skuteczny. Jest jednak wadliwy, ponieważ wymaga transmisji hasła w formie niezaszyfrowanej.

Alternatywy dla Basic Auth obejmują Digest Auth, SSL Client Auth oraz różne mechanizmy uwierzytelniania tokenowego, takie jak OAuth 2.0 i JWT. Każda z nich ma swoje plusy i minusy, ale wszystkie są bezpieczniejsze niż Basic Auth.

Szczegółowa implementacja wysyłania żądań HTTP z podstawowym uwierzytelnianiem zależy od wielu czynników, takich jak rodzaj serwera, używane technologie i wymogi bezpieczeństwa. W Swift, używamy `URLCredential` do przechowywania danych uwierzytelniania i pakietu Alamofire do obsługi żądań HTTP.

## Zobacz też:
1. [Dokumentacja Alamofire](https://github.com/Alamofire/Alamofire)
2. [RFC 1945](https://tools.ietf.org/html/rfc1945)
3. [Artkuł o uwierzytelnianiu Digest Auth](https://en.wikipedia.org/wiki/Digest_access_authentication)
4. [Informacje o OAuth 2.0](https://oauth.net/2/)
5. [JWT - JSON Web Tokens](https://jwt.io/introduction/)

---
Zauważ, że zawsze powinieneś wybierać najbezpieczniejszy i najbardziej odpowiedni sposób uwierzytelniania dla każdego konkretnego przypadku. Bezpieczeństwo cyfrowe jest bardzo ważne.