---
title:                "Extrayendo subcadenas"
html_title:           "Go: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas en Go?

Extraer subcadenas en Go es una habilidad esencial en la programación ya que permite manipular y trabajar con cadenas de texto de una manera más eficiente. Esto puede ser útil para realizar tareas como buscar y reemplazar texto, validar entradas de usuario y crear cadenas dinámicas.

## Cómo hacerlo:

Para extraer una subcadena en Go, se utiliza la función `substring` seguida de los índices en los que se encuentra la subcadena deseada dentro de la cadena original. Por ejemplo, si queremos extraer la subcadena "mundo" de la cadena "Hola mundo", usaríamos el siguiente código:

```Go
str := "Hola mundo"
substr := str[5:10]
fmt.Println(substr)
```

El resultado sería "mundo", ya que el índice 5 marca el comiendo de la subcadena y el índice 10 marca el final.

También se pueden utilizar variables para los índices, lo que permite mayor versatilidad en la extracción de subcadenas. Por ejemplo:

```Go
start := 5
end := 10
substr := str[start:end]
```

En este caso, el resultado también sería "mundo". Además, se pueden utilizar números negativos para contar desde el final de la cadena. Por ejemplo, para extraer la palabra "Hola" de la cadena "Hola mundo", se utilizaría:

```Go
substr := str[:4]
```

El resultado sería "Hola", ya que no se especifica un índice inicial y se toma el inicio de la cadena como el punto de partida.

## Profundizando:

La función `substring` en Go también puede tomar un tercer argumento opcional que indica el paso o incremento en la iteración de los índices. Por ejemplo, si queremos extraer cada segundo carácter de una cadena, se usaría:

```Go
substr := str[::2]
```

El resultado sería "Hlmn", ya que el tercer argumento indica que se salte cada segundo carácter en la cadena original mientras se extrae la subcadena. Esto podría ser útil cuando se trabaja con cadenas largas y se necesitan solo ciertas partes de ella.

Recordemos que los índices en Go comienzan desde 0, por lo que en una cadena de 10 caracteres, el índice 9 representaría el último carácter.

## Ver también:

- Documentación oficial de Go sobre el manejo de cadenas: https://golang.org/pkg/strings/
- Tutorial sobre la extracción de subcadenas en Go: https://www.digitalocean.com/community/tutorials/how-to-work-with-strings-in-go-es
- Código de ejemplo para practicar la extracción de subcadenas en Go: https://play.golang.org/p/7FRSUhHAbvA