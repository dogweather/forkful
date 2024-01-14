---
title:                "Go: Skriva tester"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av programmering, särskilt när det kommer till Go-språket. Genom att skriva tester kan du säkerställa att din kod fungerar som förväntat och undvika potentiella fel i produktion. Det hjälper också till att underlätta samarbete med andra utvecklare och gör det enklare att underhålla och vidareutveckla din kod.

## Hur man gör det

Att skriva tester i Go är enkelt och följer ett bestämt format. Här är ett exempel på hur du kan skriva en enkel testfunktion för att kontrollera om två strängar är lika:

```Go
func TestStringEquality(t *testing.T) {
    s1 := "Hello"
    s2 := "Hello"
    if s1 != s2 {
        t.Errorf("Expected s1 and s2 to be equal, but they are not.")
    }
}
```

Genom att använda funktionen `testing.T` och `t.Errorf()` kan vi rapportera något felaktigt som händer. Just här kontrollerar vi om strängarna är lika, men det är bara en av många olika metoder som kan användas för att testa din kod på olika sätt.

## Djupdykning

När du skriver tester i Go är det viktigt att förstå konceptet med "Table Driven Tests". Detta innebär att du kan testa din kod mot flera olika inputs för att säkerställa att den fungerar korrekt i olika scenarier. Här är ett exempel på hur det kan se ut:

```Go
func TestSquare(t *testing.T) {
    inputs := []struct {
        num, expected int
    }{
        {2, 4}, // test med num = 2, förväntat output = 4
        {3, 9}, // test med num = 3, förväntat output = 9
        {4, 16}, // test med num = 4, förväntat output = 16
    }
    for _, input := range inputs {
        result := Square(input.num)
        if result != input.expected {
            t.Errorf("For input %d, expected output %d but got %d", input.num, input.expected, result)
        }
    }
}
```

Genom att använda denna metod kan du snabbt och enkelt testa din kod mot olika värden och se till att den fungerar korrekt.

## Se även

- [Official Go documentation on testing](https://golang.org/pkg/testing/)
- [Table Driven Tests in Go](https://github.com/golang/go/wiki/TableDrivenTests)
- [Best practices for writing clean and effective tests in Go](https://medium.com/agrea-technogies/best-practices-for-writing-clean-go-test-code-inverse-coding-433cd73cdb7f)