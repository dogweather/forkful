---
title:                "打印调试输出"
html_title:           "C#: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

什麼是「打印調試輸出」，以及為什麼程式設計師會這麼做？

打印調試輸出是指在程式碼中插入顯示訊息的指令，以便程式設計師能夠追蹤和除錯程式碼。程式設計師通常會在編寫和測試程式碼時使用這種方法，因為它可以幫助他們理解程式的運行情況以及定位錯誤。

如何實現打印調試輸出：

```C#
Console.WriteLine("Debug output: This is a test message!"); 
```

此範例代碼將在控制台窗口中顯示「調試輸出: 這是一條測試訊息！」。其他常用的打印調試輸出指令還包括 `Debug.Print()`和`Trace.WriteLine()`。

深入探討

打印調試輸出的概念來自於早期的調試方法，如使用開關光柵機和逐步執行程式碼。現在，程式設計師可以通過打印調試輸出指令快速且有效地檢查程式碼的執行情況。

除了打印調試輸出，還有其他調試方法，如使用IDE的調試器和單元測試。這些方法可以更有效地排除程式碼錯誤，因此程式設計師應該在需要時考慮使用它們。

有關實現打印調試輸出的詳細信息，可以參考C#官方文檔和其他網上教程。

其他參考資料：

- [C#官方文檔](https://docs.microsoft.com/zh-tw/dotnet/csharp/programming-guide/file-system/how-to-write-to-a-text-file)
- [C#打印調試輸出教程](https://www.tutorialspoint.com/csharp/csharp_debugging.htm)
- [Allen Holub的調試技巧與策略](https://www.agileconnection.com/article/debugging-strategy-action)