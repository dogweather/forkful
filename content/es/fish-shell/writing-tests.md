---
title:                "Escribiendo pruebas"
date:                  2024-01-19
simple_title:         "Escribiendo pruebas"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Escribir pruebas significa crear programas que verifican que otro software funcione como se espera. Los programadores las hacen para asegurar calidad, prevenir errores y facilitar el mantenimiento del código.

## ¿Cómo hacerlo?

```Fish Shell
function test_suma
    set resultado (math 2+2)
    if test $resultado = 4
        echo "Prueba pasada: suma funciona"
    else
        echo "Prueba fallada: suma no funciona"
    end
end
```

Salida esperada:

```
Prueba pasada: suma funciona
```

Para correr todas las pruebas en un directorio:

```Fish Shell
for file in (ls tests/*.fish)
    source $file
    eval (basename $file '.fish')
end
```

## Análisis Profundo

Las pruebas automatizadas aparecieron en los 70 con "Test Driven Development" (Desarrollo Dirigido por Pruebas) popularizándose en los 90. Alternativas a Fish Shell incluyen bash o zsh para scripting o frameworks de pruebas como PyTest para Python. En Fish, las pruebas se escriben como funciones y usualmente se ejecutan manualmente o a través de un script que recorre archivos de prueba.

## Ver También

- Documentación oficial de Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Tutorial de Scripting en Fish: [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
- Artículo sobre TDD en Wikipedia: [https://es.wikipedia.org/wiki/Desarrollo_guiado_por_pruebas](https://es.wikipedia.org/wiki/Desarrollo_guiado_por_pruebas)
