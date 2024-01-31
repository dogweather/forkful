---
title:                "Usando una shell interactiva (REPL)"
date:                  2024-01-26T04:14:12.162566-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando una shell interactiva (REPL)"

category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Un REPL (Read-Eval-Print Loop, Bucle de Leer-Evaluar-Imprimir) te permite interactuar con el código en vivo; lee la entrada, la evalúa, imprime el resultado y vuelve a empezar. Los programadores lo usan para probar fragmentos de código, depurar y aprender nuevos lenguajes en tiempo real.

## Cómo hacerlo:
Go no incluye un REPL incorporado, pero puedes usar herramientas de terceros. Una herramienta popular es `gore`:

```go
// Instala gore usando
$ go install github.com/motemen/gore/cmd/gore@latest

// Ejecuta gore
$ gore
gore version 0.5.0  :help para ayuda
gore> :import fmt
gore> fmt.Println("¡Hola, REPL de Go!")
¡Hola, REPL de Go!
nil
```

## Inmersión Profunda
Originalmente desarrollados para Lisp, los REPL son comunes en lenguajes dinámicos como Python o Ruby. Go, al ser de tipado estático, no incluye uno de manera predeterminada. Alternativas a `gore` incluyen `go-pry` y `yaegi`. Estas herramientas interpretan el código de Go, permitiéndote explorar y validar ideas rápidamente sin compilar una aplicación completa. Son especialmente útiles para principiantes y en contextos educativos donde el enfoque está en aprender y experimentar.

## Ver También
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry) 
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)
