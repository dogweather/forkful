---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Comparar dos fechas es verificar cuál es mayor, menor o si son iguales. Los programadores lo hacen para ordenar elementos, calcular los períodos de tiempo, e implementar lógicas basadas en tiempo.

## ¿Cómo hacerlo?

Aquí tienes algunos ejemplos código:

- Para comparar dos fechas usaremos la función `strcmp()`:

```PHP
$fechaUno = '2020-12-15';
$fechaDos = '2020-12-10';
 
if(strcmp($fechaUno,$fechaDos)>0){
  echo "La fechaUno es mayor que la fechaDos.";
}
elseif(strcmp($fechaUno,$fechaDos)<0){
  echo "La fechaUno es menor que la fechaDos.";
}
else{
  echo "Ambas fechas son iguales.";
}
```

- Para encontrar la diferencia en días entre dos fechas, usaremos la clase `DateTime` y `diff()`:

```PHP
$fechaUno = new DateTime('2020-10-15');
$fechaDos = new DateTime('2020-12-10');
$diferencia = $fechaUno->diff($fechaDos);
echo 'La diferencia es de '.$diferencia->d.' días';
```

## Profundizamos

Históricamente, comparar fechas en PHP no siempre ha sido fácil debido a los formatos de fecha y hora. Sin embargo, la introducción de las funciones orientadas a objetos como `DateTime()` y `diff()` ha simplificado significativamente el proceso.

Sin embargo, hay otras formas. Podrías convertir las fechas en marcas de tiempo usando `strtotime()` y luego compararlas.

```PHP
$fechaUno = strtotime('2020-12-15');
$fechaDos = strtotime('2020-12-10');
 
if($fechaUno > $fechaDos){
  echo 'La fechaUno es mayor que la fechaDos.';
}
```

Este método es útil, pero no tiene la precisión de `DateTime()`. Puedes usar `DateTime()` con diferentes zonas horarias si es necesario.

Las fechas también se pueden comparar utilizando métodos de extensión como `Carbon` en `Laravel`.

## Ver también

Para más detalles, puedes referir a los siguientes enlaces:
* PHP DateTime (Oficial): https://www.php.net/manual/es/book.datetime.php
* PHP strcmp() (Oficial): https://www.php.net/manual/es/function.strcmp.php
* Comparar fechas en PHP (StackOverflow ES): https://es.stackoverflow.com/questions/887/comparar-fechas-en-php
* Carbon (Laravel): https://carbon.nesbot.com/docs/