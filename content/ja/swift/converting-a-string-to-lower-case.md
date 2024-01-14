---
title:    "Swift: 小文字への文字列の変換"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# なぜ？
文字列を小文字に変換する理由を1-2文で説明します。

## 方法
「```Swift ...```」コードブロック内にコーディングの例とサンプル出力を示します。

```Swift
// 文字列を小文字に変換する方法
let str = "Hello, World!"
let lowerStr = str.lowercased()
print(lowerStr)
// Output: hello, world!
```

## 詳細を深く掘り下げる
文字列を小文字に変換する方法は、単純な操作に思えるかもしれませんが、Swiftではいくつかの方法があります。例えば、```lowercased()```メソッドを使用する方法の他に、```String```クラスの```lowercased(with: NSLocale?)```メソッドを使用する方法もあります。これにより、ロケールに応じた小文字への変換が可能になります。

## それでは
「See Also」の見出しで、文字列操作に関するさらに深い理解を得るためのリンクをいくつか共有します。

## 他の記事
- [Swift Programming: Advanced String Manipulation](https://medium.com/@kazuhiro/suift-programming-advanced-string-manipulation-5d6b25a3c618) (英語)
- [Swiftで文字列の前後の空白を削除する方法](https://www.mitsue.co.jp/knowledge/blog/frontend/201810/02_0000.html) (日本語)
- [Swiftで文字列の文字数を数える方法](https://dev.classmethod.jp/server-side/swift-string-count/) (日本語)