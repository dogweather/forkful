---
title:                "下载网页"
html_title:           "Elm: 下载网页"
simple_title:         "下载网页"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# 为什么
两句话解释 *为什么* 有人会参与下载网页。
某些情况下，我们需要对网站进行离线浏览，或者保存网页以供以后参考。该技术可以让我们轻松下载网页，并在本地进行浏览和编辑。

# 如何
在这个技术教程中，我们将学习如何使用Elm来下载网页。我们将使用"download"库来实现这一目的。

首先，我们需要导入"download"库。我们可以使用`elm install elm/http`命令安装这个库。

接下来，我们需要定义一个命名空间来存储我们的下载代码。在这个例子中，我们将使用`Download`作为命名空间。

```elm
module Download exposing (..)

import Http

```
接下来，我们需要定义一个函数来发出HTTP请求并下载页面。我们需要提供URL和一个编码文件的部分名称作为参数。

```elm
downloadPage : String -> String -> Cmd.Cmd Msg
downloadPage url fileName =
    Http.getString url
        |> Task.andThen
            (\text ->
                let
                    file =
                        File.string fileName text

                    request =
                        File.request file
                in
                    Http.send File.downloadStarted request
            )
```
在上面的代码中，我们首先使用`Http.getString`函数发出一个HTTP请求，然后把返回的文本转换为`Task`类型。接下来，我们使用`Task.andThen`函数将其作为参数传递给一个匿名函数，该函数创建一个文件对象并将其用作请求的参数。最后，我们使用`Http.send`函数来发送一个消息，该消息将触发一个下载事件。

最后，我们需要定义一个消息处理函数，以便在下载完成后收到通知。

```elm
type Msg
    = DownloadStarted Http.Request
	| DownloadFinished
```

这里，我们定义了两种不同的消息类型。`DownloadStarted`消息将包含一个`Http.Reqest`对象，表示下载已经开始。`DownloadFinished`消息将指示下载完成。

现在我们已经准备好下载页面了。我们可以在`view`函数中使用`Html.button`来创建一个按钮，并在用户点击时调用我们之前定义的`downloadPage`函数。

```elm
view : Model -> Html.Html Msg
view model =
    Html.div []
        [
            Html.button [ Html.Events.onClick <| downloadPage "https://www.google.com/" "Google.html" ]
                [text "Download"]
        ]
```
最后，我们需要在`update`函数中处理我们定义的消息。当我们收到`DownloadFinished`消息时，我们可以显示一个成功的消息。

```elm
update : Msg -> Model -> (Model, Cmd.Cmd Msg)
update msg model =
    case msg of
        DownloadStarted request ->
            (model, Http.send DownloadFinished request)

        DownloadFinished ->
            (model, Cmd.none)
```
现在我们可以保存并尝试运行我们的应用程序。当我们点击按钮时，我们将触发一个HTTP请求，然后下载页面并将其保存为"Google.html"文件。

# 深入探讨
Elm的"download"库提供了更多的功能，例如可以自定义请求的HTTP头，以及在下载过程中进行进度跟踪。您可以在[官方文档](https://package.elm-lang.org/packages/elm/http/latest/Http#download)中了解更多信息。

# 参考链接
- [官方文档](https://package.elm-lang.org/packages/elm/http/latest/Http#download)
- [GitHub 仓库](https://github.com/elm-lang/http/tree/1.0.0)
- [Web API 规范](https://developer.mozilla.org/zh-CN/docs/Web/API/XMLHttpRequest/Using_XMLHttpRequest#%E6%8E%A5%E5%8F%A3%E6%96%B9%E6%B3%95)