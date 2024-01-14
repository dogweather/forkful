---
title:                "Go: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hvorfor
Hvis du har jobbet med å utvikle applikasjoner eller nettsteder, har du sannsynligvis støtt på behovet for å sende HTTP-forespørsler med grunnleggende autentisering. Denne typen autentisering krever at brukernavn og passord sendes i hver forespørsel for å få tilgang til et bestemt område eller ressurs. Men hvorfor trenger du å gjøre dette i Go?

# Hvordan
For å sende en HTTP-forespørsel med grunnleggende autentisering i Go, kan du bruke Go sin "net/http" pakke. Først må du sette opp en ny forespørsel ved hjelp av "http.NewRequest" funksjonen. Deretter må du legge til autentiseringsinformasjonen i "Authorization" header ved hjelp av "SetBasicAuth" funksjonen. Til slutt kan du bruke "http.Client" sin "Do" funksjon for å utføre forespørselen. Se nedenfor for et eksempel.

```Go
req, err := http.NewRequest("GET", "https://www.example.com/secret", nil)
if err != nil{
    panic(err)
}
req.SetBasicAuth("brukernavn", "passord")

resp, err := http.DefaultClient.Do(req)
if err != nil{
    panic(err)
}
defer resp.Body.Close()
fmt.Println(resp.Status)
```

Dette eksempelet viser hvordan du kan opprette en ny GET-forespørsel til en ressurs som krever grunnleggende autentisering og hvordan du kan få tilgang til ressursen ved å legge til autentiseringsinformasjonen i "Authorization" header.

# Dypdykk
Når du sender en HTTP-forespørsel med grunnleggende autentisering, må du sørge for at brukernavnet og passordet blir overført til serveren på en sikker måte. Dette kan oppnås ved hjelp av HTTPS-protokollen, som krypterer dataene før de sendes over Internett. Det er også viktig å merke seg at passordet bør være lagret i en sikker form på klientens side for å unngå uautorisert tilgang.

# Se også
- [Eksperimentell kode: Send en HTTP-forespørsel med grunnleggende autentisering](https://play.golang.org/p/J4Kq46Cl6DV)
- [Go net/http pakken dokumentasjon](https://golang.org/pkg/net/http/)
- [RFC 7617: HTTP-akseptansetest og autentiseringsstandarder for webforestillinger](https://tools.ietf.org/html/rfc7617)