---
title:                "Een HTTP-verzoek verzenden met basisauthenticatie"
date:                  2024-01-28T22:07:54.684564-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden met basisauthenticatie"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/clojure/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verzenden van een HTTP-verzoek met basisauthenticatie omvat het toevoegen van een gebruikersnaam en wachtwoord aan een verzoek voor beperkte bronnen. Programmeurs doen dit om toegang te krijgen tot API's of webservices die enig niveau van beveiliging vereisen.

## Hoe te:

In Clojure gebruik je doorgaans de `clj-http` bibliotheek voor HTTP-verzoeken, inclusief die met basisauthenticatie. Laten we beginnen met het toevoegen van de afhankelijkheid (`[clj-http "3.12.3"]` bij mijn laatste update) aan je `project.clj`.

Vervolgens is hier hoe je een GET-verzoek met basisauthenticatie opstelt:

```clojure
(require '[clj-http.client :as client])

(let [reactie (client/get "https://jouw-api.com/bron"
                           {:basic-auth ["gebruikersnaam" "wachtwoord"]})]
  (println "Status:" (:status reactie))
  (println "Body:" (:body reactie)))
```
Vervang `"https://jouw-api.com/bron"`, `"gebruikersnaam"`, en `"wachtwoord"` met jouw gegevens. Deze code verstuurt een GET-verzoek en drukt de status en body van de reactie af.

Een voorbeelduitvoer kan er ongeveer zo uitzien:

```
Status: 200
Body: {JSON data of iets anders hier}
```

## Diepgaande Duik

HTTP-basisauthenticatie heeft zijn wortels in vroege webprotocollen. Het stuurt de gebruikersnaam en het wachtwoord in een HTTP-header gecodeerd met behulp van Base64. Hoewel het eenvoudig is, is het niet het veiligste, aangezien de referenties gemakkelijk kunnen worden gedecodeerd als ze worden onderschept.

Alternatieven:
- **Digest Authenticatie**: Complexer, omvat het verzenden van een gehashte versie van het wachtwoord in plaats daarvan.
- **OAuth**: Een robuuster systeem voor autorisatie dat niet vereist dat een gebruikersnaam en wachtwoord worden verzonden.
- **API-sleutels**: Unieke tokens die worden gebruikt in plaats van traditionele inloggegevens.

Onder de motorkap in `clj-http`, het specificeren van `:basic-auth` in de opties hashmap activeert de bibliotheek om je referenties te coderen en aan de HTTP `Authorization` header toe te voegen. Wanneer de server het verzoek krijgt, decodeert deze de header en controleert de referenties.

Houd er rekening mee dat voor veilige verzending, HTTPS gebruikt moet worden om te voorkomen dat anderen je referenties onderscheppen.

## Zie Ook

- clj-http GitHub-repo: https://github.com/dakrone/clj-http
- Officiële Documentatie van Clojure: https://clojure.org/
- HTTP Authenticatie op MDN: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication 
- OAuth begrijpen: https://oauth.net/