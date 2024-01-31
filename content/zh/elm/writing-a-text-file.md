---
title:                "编写文本文件"
date:                  2024-01-19
simple_title:         "编写文本文件"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
写文本文件就是把字符数据保存到可读的文件中。程序员这么做是为了持久化数据，分享信息，或是作为配置和日志输出。

## How to: 如何操作
```Elm
-- Elm当前版本不支持直接写文件，你需要使用JavaScript交互（Ports）。
-- 下面是个示例：

-- Elm代码: Main.elm
port module Main exposing (..)

import Browser
import Html

-- 定义一个输出端口
port saveFile : String -> Cmd msg

-- 生成要写入的文本内容并发送到JS
saveTextToFile : String -> Cmd msg
saveTextToFile text =
    saveFile text

-- 主程序，一个按钮用来触发保存文件操作
main =
    Html.button [ Html.Events.onClick (saveTextToFile "hello, world") ] [ Html.text "Save Text" ]
```

```JavaScript
// JavaScript代码: index.js
// 你需要在网页中引入这个代码，以便与Elm程序交互。

var app = Elm.Main.init();

app.ports.saveFile.subscribe(function (text) {
  var blob = new Blob([text], { type: 'text/plain' });

  // 创建一个下载链接并点击它
  var dowloadLink = document.createElement("a");
  dowloadLink.download = "file.txt";
  dowloadLink.href = window.URL.createObjectURL(blob);
  dowloadLink.style.display = "none";
  document.body.appendChild(dowloadLink);
  dowloadLink.click();
  document.body.removeChild(dowloadLink);

  // 释放URL对象
  window.URL.revokeObjectURL(dowloadLink.href);
});
```

## Deep Dive: 深入了解
在早期，Elm专注于前端且无法直接进行文件操作。Elm通过端口（Ports）和JavaScript耦合，可以实现更多的功能，包括写文件。直接在Elm内部写文件暂时不支持，但通过端口可以间接完成。有些函数式编程语言，如Haskell，提供了内置的文件写入操作。

## See Also: 另请参阅
- Elm官方指南中的[端口部分](https://guide.elm-lang.org/interop/ports.html)
- [MDN的Blob文档](https://developer.mozilla.org/en-US/docs/Web/API/Blob) 包含了在JavaScript中操作文件的相关API说明
- [FileSaver.js](https://github.com/eligrey/FileSaver.js/) 一个流行的JavaScript库，简化了客户端生成文件并保存到本地的过程
