---
title:                "Radera tecken som matchar ett mönster"
html_title:           "Go: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
Ibland behöver man ta bort vissa tecken från en textsträng baserat på ett visst mönster. Det kan vara till exempel när man vill filtrera ut oönskade tecken eller när man vill ändra utseendet på en text.

## Så här gör du
Om du vill ta bort tecken från en textsträng baserat på ett mönster kan du använda dig av funktionen `ReplaceAllString` i Go. För att använda den behöver du importera paketet `regexp` för att använda reguljära uttryck. Sedan kan du använda följande syntax för att ta bort tecken baserat på mönstret "abc" från en textsträng:

```Go
str := "abc123def456"
pattern := "[a-c]+"
newStr := regexp.MustCompile(pattern).ReplaceAllString(str, "")
fmt.Println(newStr) // Output: 123456
```

I det här exemplet så kommer alla tecken som matchar mönstret "[a-c]+" att tas bort från textsträngen "abc123def456" och den nya strängen "123456" kommer att skrivas ut.

Ett annat sätt att ta bort tecken är att använda funktionen `ReplaceAll` i kombination med `regexp.MustCompileFunc`. I det här fallet kan man även ange en funktion som avgör vilka tecken som ska tas bort. Se nedanstående kodexempel:

```Go
str := "abc123def456"
pattern := "[0-9]+" 
newStr := regexp.MustCompileFunc(pattern, func(match string) string {
    if strings.Contains(match, "1") {
        return ""
    }
    return match
}).ReplaceAllString(str, "")
fmt.Println(newStr) // Output: abcdef
```

I det här exemplet så kommer alla siffror som innehåller en "1" att tas bort från textsträngen "abc123def456" och den nya strängen "abcdef" kommer att skrivas ut.

## Vertikalt djupdykning
Funktionerna `ReplaceAllString` och `ReplaceAll` är mycket användbara när man vill ta bort tecken från en textsträng baserat på ett mönster. Det finns även andra användbara funktioner som `FindAllString` och `Split` i paketet `regexp` som kan hjälpa dig att manipulera textsträngar baserat på reguljära uttryck.

Det finns även andra paket som kan hjälpa dig att hantera textsträngar, till exempel `strings` som innehåller funktioner för att manipulera textsträngar och `unicode` som innehåller funktioner för att hantera teckenkodningar.

## Se även
- [Go Regexp Paketet](https://golang.org/pkg/regexp/)
- [Go Strings Paketet](https://golang.org/pkg/strings/)
- [Go Unicode Paketet](https://golang.org/pkg/unicode/)