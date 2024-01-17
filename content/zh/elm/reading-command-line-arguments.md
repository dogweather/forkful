---
title:                "读取命令行参数"
html_title:           "Elm: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

什么和为什么？
读取命令行参数是指从命令行中获取输入的过程，程序员这样做是为了使程序更加灵活和可配置。

如何：
```Elm
import Platform exposing (worker)

main =
    worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        
        
-- 初始化函数
init flags =
    ( Model flags, Cmd.none )
    
    
-- 更新函数
update msg model =
    case msg of
        SetArgs args ->
            ( { model | args = args }, Cmd.none )

-- 订阅函数
subscriptions model =
    Sub.none

-- 视图函数
view model =
    text (toString model.args)
        
```

深入探索：
读取命令行参数在计算机编程的历史中已经存在了很长一段时间，这种方法可以帮助程序员在运行程序时通过命令行输入不同参数来调整程序的行为。也可以通过使用其他编程语言如JavaScript来实现读取命令行参数的功能。

另请参阅：
- [Elm Platform](https://elm-lang.org/)
- [JavaScript Command Line Arguments](https://www.geeksforgeeks.org/javascript-command-line-arguments/)