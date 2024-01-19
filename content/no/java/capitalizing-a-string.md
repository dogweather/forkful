---
title:                "Gjøre en streng stor"
html_title:           "Java: Gjøre en streng stor"
simple_title:         "Gjøre en streng stor"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hvordan kapitalisere en streng i Java

## Hva & Hvorfor?
Kapitalisering av en streng betyr at første bokstav i strengen blir gjort stor. Programmetører gjør dette for å forbedre lesbarheten og korrektur i en gitt tekst.

## Hvordan til:
Med Java, bruker vi `substring()` og `toUpperCase()` metoder for å kapitalisere en streng. Her er et eksempel:

```Java
public static String capitalize(String str) {
    if(str == null || str.isEmpty()) {
        return str;
    }
    return str.substring(0, 1).toUpperCase() + str.substring(1);
}

public static void main(String[] args) {
    System.out.println(capitalize("hello world!"));  // Output: Hello world!
}
```

## Deep Dive
Historisk sett, denne praksisen med kapitalisering har vært i bruk siden de tidlige dagene av programmering for å forbedre lesbarheten.

Alternativt, hvis du bruker Apache Commons lang bibliotek, kan du benytte `WordUtils.capitalize()` funksjonen for å gjøre dette. Men for standard Java, vi holder oss til `substring()` og `toUpperCase()`.

Når det gjelder implementasjonsdetaljer, `toUpperCase()` metoden konverterer alle tegnene i strengen til store bokstaver, mens `substring()` metoden henter en del av strengen. Kombinasjonen av disse to gir oss kapitalisering.

## Se Også:
- [Java String substring() Method](https://www.javatpoint.com/java-string-substring)
- [Java String toUpperCase() Method](https://www.javatpoint.com/java-string-touppercase)
- [Apache Commons Lang – WordUtils.capitalize()](https://www.baeldung.com/java-capitalizing-lowercase)