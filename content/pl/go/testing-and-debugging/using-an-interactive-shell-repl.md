---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:42.246940-07:00
description: "Jak to zrobi\u0107: Mimo \u017Ce Go nie zawiera wbudowanego REPL, spo\u0142\
  eczno\u015B\u0107 stworzy\u0142a narz\u0119dzia takie jak `gore`, aby wype\u0142\
  ni\u0107 t\u0119 luk\u0119. Najpierw zainstaluj\u2026"
lastmod: '2024-03-13T22:44:34.853069-06:00'
model: gpt-4-0125-preview
summary: "Mimo \u017Ce Go nie zawiera wbudowanego REPL, spo\u0142eczno\u015B\u0107\
  \ stworzy\u0142a narz\u0119dzia takie jak `gore`, aby wype\u0142ni\u0107 t\u0119\
  \ luk\u0119."
title: "Korzystanie z interaktywnej pow\u0142oki (REPL)"
weight: 34
---

## Jak to zrobić:
Mimo że Go nie zawiera wbudowanego REPL, społeczność stworzyła narzędzia takie jak `gore`, aby wypełnić tę lukę. Najpierw zainstaluj `gore`, uruchamiając:

```
$ go get -u github.com/motemen/gore
```

Po zainstalowaniu uruchom `gore`, wpisując `gore` w terminalu:

```
$ gore
```

Powinieneś zobaczyć monit gotowy do przyjmowania poleceń Go. Spróbujmy prostego przykładu:

```
gore> :import fmt
gore> fmt.Println("Witaj, Go REPL!")
```

Powinieneś zobaczyć wynik:

```
Witaj, Go REPL!
```

Zmienne i definicje funkcji działają jak oczekiwano. Możesz zadeklarować funkcję:

```
gore> :import math
gore> areaCircle := func(radius float64) float64 {
...> return math.Pi * radius * radius
...> }
gore> fmt.Println("Pole koła o promieniu 4:", areaCircle(4))
```

I od razu otrzymać wyjście:

```
Pole koła o promieniu 4: 50.26548245743669
```

## Dogłębna analiza:
Koncepcja REPL jest starożytna, sięgająca maszyn Lisp z lat 60., zapewniających interaktywne doświadczenie programistyczne. W przeciwieństwie do języków takich jak Python czy JavaScript, Go zostało zaprojektowane bez REPL, skupiając się zamiast tego na skompilowanych plikach wykonywalnych dla wydajności i prostoty. Odzwierciedla to filozofię prostoty Go oraz jego projektowanie dla skalowalnego i łatwego w utrzymaniu oprogramowania.

Jednakże, narzędzia takie jak `gore` czy `goplay` pokazują pomysłowość społeczności Go w zamykaniu tej luki. Te narzędzia dynamicznie analizują kod Go i używają pakietu `go/eval` lub podobnych mechanizmów do jego wykonania w czasie rzeczywistym, choć z pewnymi ograniczeniami w porównaniu do natywnego środowiska REPL. Te ograniczenia wynikają z systemu typów Go i modelu kompilacji, które mogą utrudniać ocenę w locie.

Chociaż środowiska REPL są wyjątkowo użyteczne do edukacji i szybkich testów, ekosystem Go zwykle skłania się ku tradycyjnym procesom kompilacji i uruchamiania dla większości zadań programistycznych. Środowiska IDE i edytory wspierające Go, takie jak Visual Studio Code czy GoLand, oferują zintegrowane narzędzia do testowania i debugowania, które w dużym stopniu eliminują potrzebę REPL dla profesjonalnego rozwoju.

Dla programowania eksploracyjnego, tworzenia prototypów lub nauki, REPL-e takie jak `gore` oferują jednak cenną alternatywę, umożliwiając programistom przyzwyczajonym do REPL-i w innych językach korzystanie z podobnego doświadczenia w Go.
