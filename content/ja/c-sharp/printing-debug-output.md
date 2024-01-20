---
title:                "デバッグ出力の印刷"
html_title:           "Fish Shell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

# デバッグ出力の印刷: それは何ですか、なぜそれが必要ですか？ ("What & Why?")

デバッグ出力の印刷は、プログラムがどのように実行されているかを理解するための道具です。プログラマーはこれを用いてプログラム内のエラーを特定し、修正します。

# 操作方法 ("How to:")

```C#
using System.Diagnostics;

class Program 
{
    static void Main()
    {
        Debug.WriteLine("デバッグ出力");
    }
}
```

これは単純なデバッグ出力です。上記の支援は「デバッグ出力」というテキストを出力ウィンドウに表示します。

# 掘り下げ ("Deep Dive")

デバッグ出力の概念は、プログラミングが始まって以来存在してきました。その理由はシンプルで、プログラマーがプログラムがどのように動作しているかを理解する手段を提供するからです。

代替手段としては`Console.WriteLine()`がありますが、このメソッドはリリースモードで無効にならず、ユーザーにも表示されるため通常はデバッグ目的には用いません。

デバッグ出力の機能は`System.Diagnostics`名前空間内の`Debug`クラスによって提供されます。

# 参考リンク ("See Also")

- Debug Class: [公式文書](https://docs.microsoft.com/ja-jp/dotnet/api/system.diagnostics.debug)
- Debug and Trace Classes: [MSDN Article](https://msdn.microsoft.com/en-us/library/3y86e45b.aspx)