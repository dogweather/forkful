---
title:                "コンピュータープログラミング：コマンドライン引数の読み取り"
html_title:           "Swift: コンピュータープログラミング：コマンドライン引数の読み取り"
simple_title:         "コンピュータープログラミング：コマンドライン引数の読み取り"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ
コマンドライン引数を読み取ることが重要なのか説明します。


## 方法
```Swift
// 入力したコマンドライン引数を格納する
let args = CommandLine.arguments 
// 引数の数を出力する
print("引数の数：\(args.count)")
// 各引数を出力する
for arg in args {
  print(arg)
}
```

実行結果：
```
引数の数：2
[引数1]
[引数2]
```

## 深堀り
コマンドライン引数をプログラムに渡すことで、ユーザーがプログラムの実行時に任意の値を指定できるようになります。また、引数の数や形式をチェックすることで、エラーハンドリングやバリデーションがよりスムーズに行えるようになります。詳しくは[公式ドキュメント](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)をご覧ください。

## 参考
- [CommandLine - Swift Standard Library](https://developer.apple.com/documentation/swift/commandline)
- [Swift Functions - The Basics](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [How to Create Command Line Tools with Swift](https://www.raywenderlich.com/301-directories-in-swift-part-1-getting-started)