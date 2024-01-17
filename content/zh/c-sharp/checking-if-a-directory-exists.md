---
title:                "检查目录是否存在"
html_title:           "C#: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什麼 & 為什麼？
檢查目錄是否存在指的是在程式設計中，我們經常需要確認一個目錄是否存在於電腦中。這通常是為了確保程式運行時能夠順利找到需要使用的檔案。透過檢查目錄是否存在，我們可以提前處理可能的錯誤，確保程式的效率和可靠性。

## 如何：
我們可以使用 C# 的 `Directory.Exists` 方法來檢查目錄是否存在。假設我們想要檢查一個名為 `myFolder` 的目錄是否存在，我們可以使用以下程式碼：
```C#
if(Directory.Exists("myFolder"))
{
    Console.WriteLine("目錄存在。");
}
else
{
    Console.WriteLine("目錄不存在。");
}
```
透過這樣的程式碼，我們可以在程式執行時得到相關訊息，並根據需要做出相應的處理。

## 深入探討：
檢查目錄是否存在的概念在程式設計中早有應用。在過去，我們可能會使用其他的方法，例如使用系統指令來檢查目錄是否存在。但現在透過 C# 的 `Directory.Exists` 方法，我們可以更加方便地處理這個問題。另外，我們還可以使用 `DirectoryInfo` 物件來處理較複雜的目錄操作。

## 參考資料：
- [C# Directory Class](https://docs.microsoft.com/zh-tw/dotnet/api/system.io.directory?view=net-5.0)
- [C# DirectoryInfo Class](https://docs.microsoft.com/zh-tw/dotnet/api/system.io.directoryinfo?view=net-5.0)