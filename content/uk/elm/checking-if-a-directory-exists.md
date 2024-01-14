---
title:    "Elm: Перевірка наявності каталогу"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Чому?

Здійснення перевірки існування директорії може бути корисним для унікальних випадків програмування, наприклад, якщо ви хочете переконатися, що даний шлях існує перед його використанням у вашому коді.

## Як це зробити?

```Elm
exists : String -> Bool

exists path =
    Native.Directory.exists path

main : Html msg
main =
    if exists "images" then
        Html.text "Директорія існує"
    else
        Html.text "Директорія не існує"
```
*Вхід:* "images"
*Вихід:* "Директорія існує"

*Вхід:* "documents"
*Вихід:* "Директорія не існує"

## Детальний розгляд

Код ```exists``` використовує внутрішній модуль Native.Directory, щоб перевірити існування директорії за заданим шляхом. Цей модуль використовує функцію ```dirExists``` з пакету ```elm/file``` для виконання перевірки. Якщо директорія існує, функція повертає значення True, в іншому випадку - False.

Якщо ви хочете, щоб ваша програма взаємодіяла з системними даними директорій, ви також можете використовувати функцію ```getLocaleDirs``` з пакету ```elm-explorations/filesystem```. Ця функція повертає список шляхів до локальних директорій на вашій системі. Ви також можете використовувати функцію ```listDirs```, щоб отримати всі директорії в певному каталозі.

## Дивись також

- [Native.Directory](https://package.elm-lang.org/packages/elm/file/latest/Native-Directory)
- [elm/file](https://package.elm-lang.org/packages/elm/file/latest/)
- [elm-explorations/filesystem](https://package.elm-lang.org/packages/elm-explorations/filesystem/latest/)
- [System.Directory module](https://package.elm-lang.org/packages/elm/core/latest/System-Directory)