---
title:                "Перевірка наявності директорії"
html_title:           "Bash: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що та Чому?
Перевірка існування директорії дозволяє визначити, чи є певний шлях доступний на файловій системі. Програмісти використовують це для уникнення помилок при читанні чи запису файлів.

## Як це Зробити:
Elm як чисто фронтенд-мова не має вбудованої можливості перевіряти файлову систему, оскільки працює у безпечному, ізольованому середовищі браузера. Для взаємодії з файловою системою, можна використати JavaScript через порти в Elm.

```Elm
port module Directory exposing (..)

-- Створіть порт для перевірки директорії
port checkDirectory : String -> Cmd msg

-- Підписка на результати
port directoryExists : (Bool -> msg) -> Sub msg
```

```JavaScript
// JavaScript код, що спілкується з Elm через порти
app.ports.checkDirectory.subscribe(function(directory) {
  try {
    var exists = fs.existsSync(directory); // Node.js функція
    app.ports.directoryExists.send(exists);
  } catch (error) {
    app.ports.directoryExists.send(false);
  }
});
```

## Поглиблений Розвід
У минулому, Elm міг викликати код JS безпосередньо, але з цілей безпеки це змінилося на порти. Альтернативи включають HTTP запити до сервера, який може перевірити файлову систему, або використання localStorage/Web APIs для збереження даних у браузері. Детальніше про порти: це двосторонні комунікаційні канали між Elm та JS, дозволяючи асинхронний обмін повідомленнями.

## Також Дивіться
- [Elm Official Guide on Ports](https://guide.elm-lang.org/interop/ports.html)
- [Node.js fs.existsSync() Documentation](https://nodejs.org/api/fs.html#fsexistssyncpath)
