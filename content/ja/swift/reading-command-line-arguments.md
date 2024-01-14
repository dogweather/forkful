---
title:                "Swift: コンピュータープログラミングにおける「コマンドライン引数の読み込み」"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜコマンドライン引数を読む必要があるのか

コマンドライン引数を読むことは、プログラミングにおいて非常に重要な役割を果たします。コマンドライン引数はプログラムに外部から情報を与えることができるため、柔軟性が向上します。例えば、プログラムを起動時にファイル名やオプションを指定することで、同じプログラムでも異なるファイルや設定を使用することができるようになります。

# 読み取り方

コマンドライン引数を読み取るには、`CommandLine.arguments`という配列を使用します。この配列には起動時に指定された全ての引数が含まれています。以下のようなコードを使用することで、引数を取得することができます。

```Swift
let arguments = CommandLine.arguments
print("起動時の引数: \(arguments)")
```

もし、ファイル名やオプションを指定してプログラムを実行した場合、上記のコードは以下のような出力を示します。

```
起動時の引数: ["プログラム名", "ファイル名", "-v"]
```

# さらに深く

コマンドライン引数には、多くの情報が含まれています。例えば、`CommandLine.arguments`の最初の要素には実行しているプログラムの名前が含まれています。また、特定のオプションが指定されているかどうかをチェックすることもできます。さらに、引数には文字列だけでなく、整数や浮動小数点数などのデータも含まれることがあります。

コマンドライン引数を利用することで、プログラムの柔軟性を高めることができるだけでなく、より多様な情報を取得することができます。是非、コマンドライン引数の使用方法を覚えて、あなたのプログラムに応用してください！

# 参考リンク

- [Apple Developer Documentation: CommandLine](https://developer.apple.com/documentation/swift/commandline)
- [Learn Swift: Working with Command-line Arguments](https://www.learnswiftonline.com/getting-started/working-with-command-line-arguments/)
- [Swifty Command Line Arguments Handling](https://medium.com/@marcosantadev/swifty-command-line-arguments-handling-a8420ebe95e1)