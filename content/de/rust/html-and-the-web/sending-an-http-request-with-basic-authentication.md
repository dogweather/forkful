---
date: 2024-01-20 18:02:45.037749-07:00
description: "HTTP-Anfragen mit Basic Authentication erm\xF6glichen den Zugang zu\
  \ gesicherten Ressourcen, indem sie Benutzername und Passwort in Base64-kodierter\
  \ Form mit\u2026"
lastmod: '2024-02-25T18:49:50.740225-07:00'
model: gpt-4-1106-preview
summary: "HTTP-Anfragen mit Basic Authentication erm\xF6glichen den Zugang zu gesicherten\
  \ Ressourcen, indem sie Benutzername und Passwort in Base64-kodierter Form mit\u2026"
title: HTTP-Anfragen mit Basisauthentifizierung senden
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen mit Basic Authentication ermöglichen den Zugang zu gesicherten Ressourcen, indem sie Benutzername und Passwort in Base64-kodierter Form mit der Anfrage mitsenden. Programmierer nutzen dies, um Webdienste sicher zu konsumieren oder geschützte Daten zu übertragen.

## So geht's:
Mit Rust gestaltet sich der Versand von HTTP-Requests mit Basic Authentication mithilfe der `reqwest`-Bibliothek unkompliziert. Hier ein Beispiel:

```Rust
extern crate reqwest;
extern crate base64;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = reqwest::blocking::Client::new();
    let username = "user";
    let password = "pass";
    let encoded_credentials = base64::encode(format!("{}:{}", username, password));

    let res = client.get("http://example.com")
        .header("Authorization", format!("Basic {}", encoded_credentials))
        .send()?;

    println!("Status: {}", res.status());
    println!("Headers:\n{:#?}", res.headers());
    println!("Body:\n{}", res.text()?);
    Ok(())
}
```

Der obige Code gibt den Status der Anfrage, die Antwort-Header und den Body der Antwort aus.

## Deep Dive
HTTP Basic Authentication ist ein älteres, aber dennoch weit verbreitetes Authentifizierungsprotokoll. Dabei ist es wichtig zu beachten, dass diese Methode die Anmeldeinformationen im Klartext (nach Base64-Kodierung) überträgt und daher nur über HTTPS verwendet werden sollte, um Abhören zu vermeiden.

Alternativ gibt es fortgeschrittenere Methoden wie OAuth, die anstelle von direkten Anmeldeinformationen Token verwenden. Implementierungsdetails bei Basic Authentication in Rust betreffen vor allem die korrekte Kodierung und das Setzen der Header. Die Fehlerbehandlung sollte auch bedacht werden, da Netzwerkoperationen fehlschlagen können.

## Siehe auch
- [reqwest crate Documentation](https://docs.rs/reqwest/)
- [HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
- [The Rust Programming Language](https://doc.rust-lang.org/book/)
- [base64 crate Documentation](https://docs.rs/base64/)
