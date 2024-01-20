---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么和为什么？

在计算机编程中，字符串转换为小写就是将一个字符串里的所有大写字母改变成相应的小写字母。程序员们之所以要这样做，主要是为了实现数据的统一、准确比对和排序。

## 怎么做：

下面的代码块展示了如何在Elm中实现字符串转小写的函数。您可以将任意的字符串输入到函数中，然后函数会返回一个全由小写字母组成的新字符串。

```Elm
import String

lowerCaseString : String -> String
lowerCaseString str =
    String.toLower str

-- 测试
lowerCaseString "HELLO, ELM!"
-- 输出: "hello, elm!"
```

当然你还可以在实际的Elm应用中使用这个功能，例如处理用户输入：
```Elm
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import String

type alias Model = { data : String }

initialModel : Model
initialModel =
    { data = "" }

type Msg = LowerCase

update : Msg -> Model -> Model
update msg model =
    case msg of
        LowerCase ->
            { model | data = String.toLower model.data }

view : Model -> Html Msg
view model =
    div []
        [ text model.data
        , button [ onClick LowerCase ] [ text "转小写" ]
        ]

main =
    Browser.sandbox { init = initialModel, update = update, view = view }
```
## 深入探讨：

字符串转小写在很多编程语言中都是一个常用的基本功能，由于其实现逻辑简单，所以早在计算机发展的初期就已经被广泛使用。在Elm这门语言中，其实现主要使用了Unicode字符集和相关标准的规定。其替代方式可以通过遍历字符串中的每一个字符，然后将每个大写字符替换为对应的小写字符来实现。

## 参见：

要获取更多Elm字符串操作的相关信息，可以参阅以下链接：

- Elm官方文档：https://package.elm-lang.org/packages/elm/core/latest/String
- Elm中的字符串操作教程：http://elmprogramming.com/strings.html