---
title:                "Escribiendo pruebas"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir pruebas es verificar que tu código funciona como esperas. Los programadores las realizan para asegurar calidad y evitar fallos futuro.

## Cómo Hacerlo:
Escribimos un script simple `test.sh` que prueba un ejemplo `sumar.sh`.

`sumar.sh`:
```Bash
#!/bin/bash
resultado=$(($1 + $2))
echo $resultado
```

`test.sh`:
```Bash
#!/bin/bash
test_sumar() {
    resultado=$(./sumar.sh 3 4)
    if [ "$resultado" -eq 7 ]; then
        echo "Prueba pasada: Suma correcta"
    else
        echo "Prueba fallida: Suma incorrecta"
    fi
}

test_sumar
```

Ejecución y salida:
```Bash
$ chmod +x sumar.sh test.sh
$ ./test.sh
Prueba pasada: Suma correcta
```

## Profundización
Las pruebas comenzaron como cheques manuales pero han evolucionado a frameworks automatizados como `Bash Automated Testing System` (BATS). Alternativas a pruebas en Bash incluyen `shUnit2` o integrar Bash con herramientas como `Jenkins`. Detalles de implementación varían desde simples condiciones `if` hasta pruebas de regresión y pruebas de integración.

## Ver También
- Bash Automated Testing System (BATS): https://github.com/bats-core/bats-core
- shUnit2: https://github.com/kward/shunit2
- Jenkins: https://www.jenkins.io/
