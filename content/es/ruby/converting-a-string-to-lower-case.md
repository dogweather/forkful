---
title:    "Ruby: Convirtiendo una cadena a minúsculas"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas en Ruby? 
La mayoría de las veces, es necesario manipular y procesar cadenas en un programa. Converting a lowercase unikely erá útil si se desea realizar una comparación case-insensitive o si se necesita tener consistentemente una cadena en minúsculas para su procesamiento.

## Cómo hacerlo: 
El proceso para convertir una cadena a minúsculas en Ruby es muy sencillo. Simplemente se utiliza el método `downcase` en la cadena deseada. Aquí hay un ejemplo de código y su salida correspondiente: 
```Ruby 
cadena = "¡HOLA MUNDO!" 
puts cadena.downcase 
``` 
Salida: `¡hola mundo!`

## Profundizando:
Ahora, profundizando un poco más en el proceso de conversión de cadenas a minúsculas en Ruby: el método `downcase` crea una nueva cadena con todos los caracteres en minúsculas. Esto significa que la cadena original no se modificará, pero se puede asignar a una variable para usarla en el futuro. Además, si hay caracteres no alfabéticos en la cadena, se mantendrán en su estado original. Por ejemplo, si se tiene una cadena con un símbolo o un número, estos caracteres no se convertirán a minúsculas. 

Es importante tener en cuenta que el método `downcase` solo funciona en caracteres ASCII, no en caracteres Unicode. Por lo tanto, si se trabaja con idiomas que usan caracteres especiales, este método puede no funcionar como se espera.

## Ver también: 
- [Documentación oficial de Ruby sobre el método `downcase`](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)
- [Ejemplos adicionales de código para convertir cadenas a minúsculas en Ruby](https://www.rubyguides.com/2015/05/ruby-string-methods/)
- [Artículo sobre el manejo de cadenas en Ruby](https://www.rubyguides.com/2019/03/ruby-strings/)