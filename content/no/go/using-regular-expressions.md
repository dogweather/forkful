---
title:                "Bruk av regulære uttrykk"
date:                  2024-01-19
simple_title:         "Bruk av regulære uttrykk"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions, ofte kalt regex, er et kraftig verktøy for å søke og manipulere tekst basert på mønstre. De hjelper programmerere å matche, finne og håndtere komplekse tekststrenger effektivt.

## How to:
### Basic Match
```
package main

import (
    "fmt"
    "regexp"
)

func main() {
    re := regexp.MustCompile(`\d+`)
    fmt.Println(re.FindString("123ABC456"))
}
```
Output:
```
123
```

### Replace Text
```
package main

import (
    "fmt"
    "regexp"
)

func main() {
    re := regexp.MustCompile(`\d+`)
    fmt.Println(re.ReplaceAllString("123ABC456", "NUM"))
}
```
Output:
```
NUMABCNUM
```

### Complex Pattern Matching
```
package main

import (
    "fmt"
    "regexp"
)

func main() {
    re := regexp.MustCompile(`(?i)hello\s(\w+)`)
    match := re.FindStringSubmatch("Hello Gopher! What's up?")
    if match != nil {
        fmt.Printf("Found greeting for: %s", match[1])
    }
}
```
Output:
```
Found greeting for: Gopher
```

## Deep Dive
Regex har sine røtter tilbake til 1950-tallet og har vært en del av programmeringsspråk som Perl og Java. I Go er regex implementert i `regexp` pakken; den følger RE2-syntaks og balanserer hastighet med fleksibilitet. Alternativer inkluderer biblioteker som oniguruma for mer kompleks funksjonalitet men til kostnad av ytelse.

## See Also
- [Go’s regexp/pkg documentation](https://pkg.go.dev/regexp)
- [A Tour of Go: Regular Expressions](https://tour.golang.org/moretypes/23)
- [RE2 syntax documentation](https://github.com/google/re2/wiki/Syntax)
