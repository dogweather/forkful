---
date: 2024-01-20 18:03:26.817411-07:00
description: "How to: (Jak to zrobi\u0107:) ."
lastmod: '2024-03-13T22:44:35.754794-06:00'
model: gpt-4-1106-preview
summary: .
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
weight: 45
---

## How to: (Jak to zrobić:)
```Swift
import Foundation

// Tworzymy URL żądania
guard let url = URL(string: "https://example.com/api/data") else {
    fatalError("Invalid URL")
}

// Tworzy podstawowe dane uwierzytelniające
let login = "myUsername"
let password = "myPassword"
let loginString = "\(login):\(password)"
guard let loginData = loginString.data(using: .utf8) else { return }
let base64LoginString = loginData.base64EncodedString()

// Przygotowujemy żądanie
var request = URLRequest(url: url)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

// Wysyłamy żądanie i odbieramy odpowiedź
let config = URLSessionConfiguration.default
let session = URLSession(configuration: config)
let task = session.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data, let stringData = String(data: data, encoding: .utf8) {
        print("Otrzymane dane: \(stringData)")
    }
}

task.resume()
```

Output (Przykładowe wyjście):
```
Otrzymane dane: {"przykład": "wartość"}
```

## Deep Dive (Dogłębna analiza):
Podstawowe uwierzytelnienie HTTP (Basic Authentication) to stary, ale prosty sposób uwierzytelniania, gdzie login i hasło są kodowane base64 i dołączane do każdego żądania. Nie jest to najbezpieczniejsza metoda (w porównaniu np. do OAuth), ale nadal jest używana w wielu API, gdzie łatwość implementacji gra rolę. W nowoczesnych aplikacjach często stosuje się HTTPS, co minimalizuje ryzyko wycieku danych. Podstawowe uwierzytelnienie jest częścią standardu HTTP i można je stosować zarówno w requestach GET, jak i POST.

Alternatywnymi metodami uwierzytelnienia są tokeny (np. Bearer token), które są bardziej bezpieczne, jednakże mogą być bardziej skomplikowane w implementacji. Głównym zagrożeniem w podstawowym uwierzytelnieniu jest to, że dane użytkownika nie są szyfrowane na poziomie żądania HTTP i łatwo jest je przechwycić bez użycia HTTPS.

Ważne jest również, aby używać bezpiecznych sposobów przechowywania danych uwierzytelniających w aplikacji, unikać twardego kodowania loginów i haseł, a w miarę możliwości stosować metody zapewniające większe bezpieczeństwo, takie jak uwierzytelnienie dwuskładnikowe czy certyfikaty klienckie.

## See Also (Zobacz również):
- [HTTP Basic authentication (MDN)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [Swift and URLSession official documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [Securing Swift applications](https://www.raywenderlich.com/7181017-securing-ios-data-at-rest)
- [Understanding authentication with HTTP (HTTP Authentication: Basic and Digest Access Authentication, RFC 2617)](https://tools.ietf.org/html/rfc2617)
