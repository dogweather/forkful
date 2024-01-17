---
title:                "Generowanie losowych liczb"
html_title:           "PowerShell: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?: Generowanie losowych liczb to proces, w którym programy są wykorzystywane do tworzenia kolejnych wartości, które są naprawdę losowe. Programiści korzystają z tego narzędzia w celu uzyskania przypadkowych danych lub do testowania swojego kodu.

## Jak to zrobić?: W PowerShell można wygenerować losowe liczby w prosty i szybki sposób za pomocą wbudowanej komendy Get-Random. Poniżej znajduje się przykład kodu, który wygeneruje piętnaście losowych liczb z zakresu od 1 do 100 oraz wyświetli je na ekranie. 

```PowerShell
Get-Random -Count 15 -Minimum 1 -Maximum 100
```

**Output:** 57, 72, 13, 30, 64, 10, 45, 86, 1, 41, 18, 94, 61, 79, 29

## Głębsza analiza: Generowanie losowych liczb jest techniką, która jest wykorzystywana od dawna w programowaniu. W przeszłości programiści używali złożonych algorytmów do generowania liczb, ale dzięki postępowi technologii, można teraz korzystać z wbudowanych funkcji, takich jak Get-Random w PowerShell. Alternatywnymi sposobami na generowanie losowych liczb są także wykorzystanie dedykowanych modułów lub wykorzystanie API dostępnych w różnych językach programowania.

Co więcej, warto również zwrócić uwagę na to, że losowe liczby generowane przez komputery są w rzeczywistości pseudolosowe, ponieważ wynik jest określany przez algorytm komputera i ziarno (eng. seed) wybrane przez użytkownika. Dzięki temu można uzyskać takie same losowe liczby w przypadku ponownego uruchomienia tego samego kodu z tym samym ziarnem.

## Zobacz również: Jeśli chcesz dowiedzieć się więcej o generowaniu losowych liczb w PowerShell, możesz zapoznać się z oficjalną dokumentacją firmy Microsoft [tutaj](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.1). Możesz także przetestować różne sposoby wykorzystania komendy Get-Random i zobaczyć, jakie wyniki uzyskasz.