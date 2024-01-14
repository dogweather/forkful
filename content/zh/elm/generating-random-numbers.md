---
title:    "Elm: 随机数生成"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，生成随机数是非常有用的。它可以用来模拟真实世界的情况，也可以用来测试代码的可靠性。使用Elm编程语言，我们可以轻松的生成随机数来完成这些任务。

## 怎么做

首先，我们需要在代码中导入`Random`模块。然后，在需要生成随机数的函数中，使用`Random.generate`函数，并在其中定义一个`Random`值生成器，它会使用`Random.map`函数将生成的随机数传递给我们需要的函数。下面是一个例子：

```Elm
import Random

-- 生成一个介于1到10之间的随机数
randomNumber : Int
randomNumber =
    Random.generate (Random.int 1 10) (\number -> number)
```

上面的例子中，我们使用`Random.int`函数来生成一个1到10之间的整数。你也可以根据自己的需要使用不同的随机数生成函数，比如`Random.float`来生成浮点数。一旦生成了随机数，我们就可以将其传递给一个函数，比如将其打印出来。在下面的代码块中，我们会将生成的随机数打印在页面上：

```Elm
-- 打印随机数
printRandomNumber : Int -> Html msg
printRandomNumber num =
    text (toString num)

view : Html msg
view =
    div []
        [ h1 [] [ text "随机数生成器" ]
        , button [ onClick (Random.generate (Random.int 1 10) printRandomNumber) ] [ text "生成随机数" ]
        ]
```

运行代码后，每次点击按钮，就会生成一个新的随机数并将其打印在页面上。

## 深入探讨

在Elm中，随机数是通过一个`Seed`值来生成的。`Seed`值可以被理解为一个种子，它会影响随机数的生成过程。同一种子会生成相同的随机数，因此我们可以使用相同的种子来保证我们的代码可以复现。同时，`Seed`值也可以通过`Random.initialSeed`函数来手动创建，这样我们就可以在代码中使用预先设定好的种子来生成随机数。

## 参考链接

- [Elm官方文档: Random模块](https://package.elm-lang.org/packages/elm/random/latest/)
- [Elm入门随机数生成](https://www.hackingwithcodelondon.com/learn/elm/generating-random-numbers/)
- [Elm随机数生成器原理](https://www.beyond-tinker.com/2017/05/generate-some-random-numbers-with-elm/)
 
# 参见