---
title:    "Haskell: Escribiendo en el error estándar"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar (stderr) es una práctica común en la programación en Haskell. Se utiliza para imprimir mensajes de error y depuración, lo cual puede ser muy útil durante el desarrollo de un programa.

## Cómo hacerlo

Para escribir a stderr en Haskell, se utiliza la función "error" seguida por el mensaje de error que se desea imprimir. Por ejemplo:

```Haskell
error "¡Error: no se puede dividir por cero!"
```

La salida de este código sería:

```
¡Error: no se puede dividir por cero!
```

## Profundizando

Además de utilizar la función "error", también se puede escribir a stderr utilizando la función "hPutStrLn" del módulo "System.IO". Esta función toma como argumento una manija (handle) y una cadena de texto y escribe dicha cadena en el handle dado. Por ejemplo:

```Haskell
import System.IO

main = do
  hPutStrLn stderr "Este mensaje se escribirá a stderr."
  hPutStrLn stderr "Puedes utilizar esta función varias veces para escribir múltiples mensajes."
```

La salida de este programa sería:

```
Este mensaje se escribirá a stderr.
Puedes utilizar esta función varias veces para escribir múltiples mensajes.
```

## Ver también

- [Documentación de la función "error" de Haskell](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-3100003.16)
- [Documentación de la función "hPutStrLn" de Haskell](https://www.haskell.org/onlinereport/haskell2010/haskell2010-html/libraries/base-4.3.1.0/System-IO.html#v:hPutStrLn)