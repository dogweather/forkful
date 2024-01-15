---
title:                "Analizando html"
html_title:           "Java: Analizando html"
simple_title:         "Analizando html"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/parsing-html.md"
---

{{< edit_this_page >}}

# Por qué: 
¿Alguna vez te has preguntado cómo hacer uso de la información de una página web en tu programa? Bueno, ¡parsear HTML es la respuesta! Es una habilidad útil que te permite extraer datos específicos de una página web y utilizarlos en tu aplicación.

## Cómo hacerlo:
Para empezar, necesitas una biblioteca de Java llamada Jsoup, que facilita el proceso de parsear HTML. Aquí te mostramos un ejemplo sencillo de cómo usarla:

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class ParseadorHTML {

    public static void main(String[] args) throws Exception {
        String url = "https://www.ejemplo.com";
        Document doc = Jsoup.connect(url).get();
        Elements elementos = doc.select("h1"); //selecciona todos los elementos <h1> en la página
        for (Element elemento : elementos) {
            System.out.println(elemento.text()); //imprime el texto del elemento
        }
    }
}
```

Al ejecutar este código, obtendrás como resultado todos los encabezados de nivel 1 (<h1>) en la página de ejemplo. Pero no solo puedes seleccionar elementos por su etiqueta, también puedes utilizar selectores CSS, lo que hace que el parsing de HTML sea aún más flexible.

## Profundizando:
Para aquellos interesados en conocer más sobre cómo funciona el parsing de HTML, aquí hay algunos conceptos adicionales que pueden resultar útiles.

### DOM:
DOM (Document Object Model) es una representación en memoria de una página HTML, que permite acceder y manipular los elementos en la página. Jsoup utiliza una implementación simplificada de DOM para hacer el proceso de parsing más fácil para los desarrolladores.

### Selectores CSS:
Como mencionamos anteriormente, puedes utilizar selectores CSS para seleccionar elementos específicos en una página web. Algunos ejemplos de selectores CSS son:

- ```.clase``` para seleccionar elementos con una clase específica
- ```#id``` para seleccionar elementos con un id específico
- ```elemento[attr="valor"]``` para seleccionar elementos con un atributo y valor específico

### Documento HTML válido:
Jsoup solo puede parsear documentos HTML válidos, lo que significa que si te encuentras con un error al intentar parsear una página, es posible que el HTML no esté bien formateado. Puedes utilizar una herramienta en línea como W3C Validator para verificar la validez de un documento HTML.

## Ver también:
- [Documentación de Jsoup](https://jsoup.org/)
- [Tutorial de parsing de HTML en Java](https://www.programcreek.com/2011/03/java-parse-html-document-sample-code/)
- [Ejemplo de parsing de HTML utilizando Jsoup](https://www.baeldung.com/java-with-jsoup)