---
title:                "Usando una shell interactiva (REPL)"
aliases: - /es/php/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:16:20.307341-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando una shell interactiva (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Una shell interactiva, o REPL (Bucle Leer-Evaluar-Imprimir), te permite escribir y ejecutar código PHP al momento. Es ideal para experimentar, depurar, o aprender, ya que puedes probar fragmentos de código sin la necesidad de crear un guion completo.

## Cómo:
Lanza el REPL de PHP ejecutando `php -a` en tu terminal. Aquí tienes un ejemplo de cómo funciona:

```php
php > echo "¡Hola, Mundo!";
¡Hola, Mundo!
php > $arr = [1, 2, 3];
php > print_r($arr);
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

También puedes definir funciones:

```php
php > function suma($a, $b) { return $a + $b; }
php > echo suma(5, 10);
15
```

## Profundización
Los REPL han existido de alguna forma desde los primeros días de LISP en la década de 1960. La shell interactiva de PHP es menos avanzada en comparación con las de lenguajes como Python o JavaScript. No mantiene el estado entre sesiones y carece de características como la autocompletación. Para un REPL de PHP más rico en características, considera alternativas como `psysh` o `boris`. Estas shells de terceros ofrecen mejores herramientas de introspección, autocompletación de pestañas y hasta un depurador.

Detrás de escena, el REPL de PHP funciona compilando y ejecutando cada línea de código a medida que se introduce. Las limitaciones de este enfoque se hacen evidentes con cosas como la redeclaración de clases, lo cual no es posible en la misma sesión. Es genial para pruebas simples pero puede volverse engorroso para tareas complejas.

## Ver También
- [Manual de PHP - Shell interactiva](https://www.php.net/manual/es/features.commandline.interactive.php)
- [PsySH: Una consola de desarrollador en tiempo de ejecución, depurador interactivo y REPL para PHP](https://psysh.org/)
- [Boris: Un pequeño REPL para PHP](https://github.com/borisrepl/boris)
