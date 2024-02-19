---
aliases:
- /pl/go/concatenating-strings/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:14.282105-07:00
description: "Konkatenacja \u0142a\u0144cuch\xF3w znak\xF3w polega na po\u0142\u0105\
  czeniu dw\xF3ch lub wi\u0119cej \u0142a\u0144cuch\xF3w znak\xF3w koniec-ko\u0144\
  cem, aby utworzy\u0107 nowy \u0142a\u0144cuch. Programi\u015Bci robi\u0105 to, aby\u2026"
lastmod: 2024-02-18 23:08:49.087955
model: gpt-4-0125-preview
summary: "Konkatenacja \u0142a\u0144cuch\xF3w znak\xF3w polega na po\u0142\u0105czeniu\
  \ dw\xF3ch lub wi\u0119cej \u0142a\u0144cuch\xF3w znak\xF3w koniec-ko\u0144cem,\
  \ aby utworzy\u0107 nowy \u0142a\u0144cuch. Programi\u015Bci robi\u0105 to, aby\u2026"
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konkatenacja łańcuchów znaków polega na połączeniu dwóch lub więcej łańcuchów znaków koniec-końcem, aby utworzyć nowy łańcuch. Programiści robią to, aby dynamicznie generować tekst, takie jak konstruowanie wiadomości, ścieżek lub złożonych zapytań, co sprawia, że programy są bardziej interaktywne i elastyczne.

## Jak to zrobić:

W języku Go istnieje kilka sposobów na konkatenację łańcuchów znaków. Oto przykłady niektórych popularnych metod:

### Użycie operatora `+`:
Najprostszym sposobem na konkatenację łańcuchów znaków jest użycie operatora `+`. Jest to proste, ale nie najbardziej wydajne przy łączeniu wielu łańcuchów.
```go
firstName := "John"
lastName := "Doe"
fullName := firstName + " " + lastName
fmt.Println(fullName) // John Doe
```

### Użycie `fmt.Sprintf`:
Do formatowania łańcuchów znaków z zmiennymi bardzo przydatne jest `fmt.Sprintf`. Daje to większą kontrolę nad formatem wyjściowym.
```go
age := 30
message := fmt.Sprintf("%s ma %d lat.", fullName, age)
fmt.Println(message) // John Doe ma 30 lat.
```

### Użycie `strings.Builder`:
Do konkatenacji wielu łańcuchów znaków, zwłaszcza w pętlach, `strings.Builder` jest wydajne i zalecane.
```go
var builder strings.Builder
words := []string{"hello", "world", "from", "go"}

for _, word := range words {
    builder.WriteString(word)
    builder.WriteString(" ")
}

result := builder.String()
fmt.Println(result) // hello world from go 
```

### Użycie `strings.Join`:
Kiedy masz ciąg łańcuchów, które mają być połączone z określonym separatorem, `strings.Join` jest najlepszą opcją.
```go
elements := []string{"path", "to", "file"}
path := strings.Join(elements, "/")
fmt.Println(path) // path/to/file
```

## Zanurzenie się głębiej

Konkatenacja łańcuchów znaków, mimo że wydaje się być operacją prostoliniową, dotyka głębszych aspektów tego, jak Go obsługuje łańcuchy znaków. W Go łańcuchy są niezmienne; oznacza to, że każda operacja konkatenacji tworzy nowy łańcuch. Może to prowadzić do problemów z wydajnością podczas łączenia dużej liczby łańcuchów lub robienia tego w ciasnych pętlach, ze względu na częste alokowanie i kopiowanie pamięci.

Historycznie, języki programowania radziły sobie z niemiennością łańcuchów i wydajnością konkatenacji na różne sposoby, a podejście Go z `strings.Builder` i `strings.Join` dostarcza programistom narzędzi, które balansują pomiędzy łatwością użycia a wydajnością. Typ `strings.Builder`, wprowadzony w Go 1.10, jest szczególnie godny uwagi, ponieważ zapewnia wydajny sposób budowania łańcuchów bez ponoszenia nadmiernych kosztów alokacji wielu łańcuchów. Robi to poprzez alokowanie bufora, który rośnie w miarę potrzeb, do którego dołączane są łańcuchy.

Mimo tych opcji, kluczowe jest wybranie odpowiedniej metody w zależności od kontekstu. Dla szybkich lub rzadkich konkatenacji, proste operatory lub `fmt.Sprintf` mogą wystarczyć. Jednak dla ścieżek krytycznych pod względem wydajności, zwłaszcza tam, gdzie zachodzi wiele konkatenacji, korzystanie z `strings.Builder` lub `strings.Join` może być bardziej odpowiednie.

Chociaż Go oferuje solidne wbudowane możliwości manipulacji łańcuchami znaków, istotne jest, aby pozostać świadomym podstawowych charakterystyk wydajności. Alternatywy takie jak konkatenacja za pomocą `+` lub `fmt.Sprintf` dobrze sprawdzają się dla prostoty i operacji na mniejszą skalę, ale zrozumienie i wykorzystanie bardziej wydajnych praktyk budowania łańcuchów znaków w Go zapewnia, że aplikacje pozostają wydajne i skalowalne.
