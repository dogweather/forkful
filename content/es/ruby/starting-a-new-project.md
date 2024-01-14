---
title:    "Ruby: Comenzando un nuevo proyecto"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

#Por qué empezar un nuevo proyecto en Ruby

Si está interesado en aprender un nuevo lenguaje de programación o quiere expandir sus habilidades existentes, Ruby es una excelente opción. Con una sintaxis fácil de leer y una amplia gama de librerías y herramientas disponibles, Ruby puede ser utilizado en diferentes tipos de proyectos, desde aplicaciones web hasta scripts de automatización.

#Cómo empezar un proyecto en Ruby

Primero, asegúrese de tener Ruby instalado en su computadora. Puede descargar la última versión en la página oficial de Ruby o utilizar un gestor de versiones como RVM o rbenv.

Una vez que tenga Ruby instalado, puede crear un nuevo proyecto con el comando `mkdir` seguido del nombre de su proyecto. Luego, puede entrar al directorio del proyecto con `cd nombreDeProyecto` y crear un archivo `Gemfile` con el comando `bundle init`. Un Gemfile es un archivo que contiene las dependencias de su proyecto y le permite instalarlas y manejarlas fácilmente.

A continuación, puede agregar las librerías que necesita a su Gemfile y luego ejecutar `bundle install` para instalarlas. Una vez que haya instalado todas sus dependencias, puede comenzar a escribir su código en un archivo `.rb` en el directorio de su proyecto. Puede usar el comando `ruby nombreDeArchivo.rb` para ejecutar su código.

Veamos un ejemplo de un programa de bienvenida en Ruby:

```Ruby
#Crea un nuevo objeto llamado "nombre" para obtener el nombre del usuario
nombre = gets.chomp 

#Saluda al usuario
puts "¡Bienvenido #{nombre}! ¿Cómo estás?"
```

Si ejecutamos este código y escribimos nuestro nombre, veremos la siguiente salida:

```
> ruby bienvenida.rb
Carlos
¡Bienvenido Carlos! ¿Cómo estás?
```

#Profundizando en la creación de un proyecto

Una vez que tenga un proyecto en Ruby en funcionamiento, puede continuar aprendiendo más sobre el lenguaje y sus características. Puede explorar diferentes librerías y herramientas disponibles, como Ruby on Rails para el desarrollo de aplicaciones web, y aprender cómo utilizarlas en su proyecto.

También es importante escribir un código limpio y eficiente, por lo que familiarizarse con las convenciones de estilo de Ruby y practicar buenas prácticas de programación es esencial.

Además, es útil aprender cómo utilizar herramientas de gestión de proyectos, control de versiones y pruebas, como Git, para mantener su proyecto organizado y funcional.

#Ver también

- [Página oficial de Ruby](https://www.ruby-lang.org/es/)
- [Ruby on Rails - Tutorial de Rails para Principiantes](https://www.railstutorial.org/es/)
- [Convenciones de estilo de Ruby](https://github.com/rubocop-hq/ruby-style-guide)
- [Documentación de Git](https://git-scm.com/doc)