---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese en tekstfil betyr å programmere programvaren for å hente og tolke data fra en ekstern fil. Dette er nødvendig for å lagre, hente og manipulere data effektivt.

## Hvordan gjør man det:

Desverre og det er et resultat av Elm's filosofi om renhet og enkelhet, den tillater ikke lesing fra filsystemet direkte. Dette er fordi Elm er ment å brukes hovedsakelig i nettlesermiljøer, der direkte filsystemtilgang er begrenset av sikkerhetshensyn. Men for å lese tekstfiler, kan du bruke en omvei ved å bruke Elm med NPM pakker som inneholder andre Elm biblioteker.

```Elm
-- Forutsatt at du har lastet ned og importert 'elm/file' via NPM
import File exposing (File)

type Msg
    = GotFile (Result File.Error String)

selectFile : Cmd Msg
selectFile =
    File.Select.text GotFile
```

## Dyp Dykk:

Historisk sett har Elm lagt vekt på renhet og enkelhet, hvilket har ført til at mange tradisjonelle operasjoner (eksempelvis fillesing) utelates eller begrenses. Alternativer til Elm for lesing av tekstfiler inkluderer andre programmeringsspråk som har full tilgang til filsystemet, som Python eller JavaScript.

Når det gjelder implementeringsdetaljer, er Elm's manglende filsystemfunksjonalitet utformet for å øke robustheten mot sikkerhetstrusler. I nettlesermiljøer er direkte filsystemtilgang begrenset for å hindre at skadelig kode får tilgang til sensitive data.

## Se Også:

- [Python File I/O](https://docs.python.org/3/tutorial/inputoutput.html)
- [Node.js Filesystem Modul](https://nodejs.org/api/fs.html)