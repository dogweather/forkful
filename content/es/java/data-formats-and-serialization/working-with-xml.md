---
title:                "Trabajando con XML"
date:                  2024-01-26T04:32:20.412636-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/working-with-xml.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Trabajar con XML implica analizar, consultar y manipular documentos XML con Java. Los programadores lo hacen para el intercambio de datos, la gestión de configuración y porque muchos sistemas heredados y APIs se comunican usando XML.

## Cómo hacerlo:
Java proporciona APIs como DOM (Modelo de Objeto de Documento), SAX (API Simple para XML) y StAX (API de Transmisión para XML) para trabajar con XML. Aquí hay un ejemplo de DOM para analizar un archivo XML:

```java
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class XmlParser {
    public static void main(String[] args) {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse("data.xml");
            
            doc.getDocumentElement().normalize();
            NodeList nodeList = doc.getElementsByTagName("employee");
            
            for (int i = 0; i < nodeList.getLength(); i++) {
                Element element = (Element) nodeList.item(i);
                System.out.println("Nombre: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Edad: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Supongamos que `data.xml` se ve así:

```xml
<employees>
    <employee>
        <name>Jane Doe</name>
        <age>30</age>
    </employee>
    <employee>
        <name>John Doe</name>
        <age>40</age>
    </employee>
</employees>
```

La salida sería:

```
Nombre: Jane Doe
Edad: 30
Nombre: John Doe
Edad: 40
```

## Profundización
XML existe desde finales de los '90, proporcionando una manera estructurada y flexible de intercambiar datos entre diferentes sistemas. Aunque JSON se ha vuelto más popular para las nuevas web APIs debido a su sintaxis más simple e integración estrecha con JavaScript, XML sigue siendo ampliamente utilizado en entornos empresariales, servicios web basados en SOAP y estándares de documentos como Office Open XML para Microsoft Office.

Cuando se trata de analizar XML en Java, la API de DOM es excelente para documentos más pequeños: es basado en árboles y permite acceso completo a la estructura XML en memoria. Sin embargo, para archivos más grandes, puede ser intensivo en el uso de memoria. SAX y StAX son más amigables con la memoria, ya que son impulsados por eventos y basados en transmisión respectivamente, pero pueden ser menos convenientes para navegar estructuras XML.

Para crear o modificar XML, Java también proporciona los paquetes javax.xml.transform y javax.xml.bind (JAXB). JAXB fue parte de Java SE hasta la versión 10, luego se convirtió en una biblioteca separada debido a la eliminación de los módulos de Java EE de Java SE. Es una manera dirigida por anotaciones para serializar objetos Java a XML y viceversa.

## Ver También
Consulta estas fuentes relacionadas para más sobre trabajar con XML en Java:
- [Java API para el Procesamiento de XML (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Arquitectura Java para el Enlace de XML (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Guía de Oracle para XML en Java](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [Tecnología XML de W3C](https://www.w3.org/standards/xml/)
- [Stack Overflow: Preguntas etiquetadas 'java' y 'xml'](https://stackoverflow.com/questions/tagged/java+xml)
