---
title:                "C#: コンピュータープログラミングの記事タイトル：コマンドライン引数の読み取り"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜコマンドライン引数を読み取るのか

コマンドライン引数を読み取ることは、特定のプログラムを実行する際に非常に便利です。たとえば、ファイル名やディレクトリ、接続情報などをプログラムに渡すことができます。このように、コマンドライン引数を使用することで、プログラムをより柔軟に動作させることができます。

# 使い方

コマンドライン引数を読み取るには、C#に用意されている```Main```メソッドを使用します。このメソッドには、```string[] args```という名前のパラメータがあります。このパラメータには、実行時に渡されるコマンドライン引数が格納されています。

例えば、以下のようなコードを実行すると、```Hello World```というメッセージとともに、渡されたコマンドライン引数が出力されます。

```C#
static void Main(string[] args)
{
    Console.WriteLine("Hello World!");
    Console.WriteLine("コマンドライン引数: " + args[0]);
}
```

入力するコマンド：```dotnet run test``` 

出力：
```
Hello World!
コマンドライン引数: test
```

# 深堀り

コマンドライン引数は、プログラム実行時に変更される可能性があるため、注意が必要です。```args```配列には常に最低でも1つの要素が含まれていることを確認する必要があります。また、コマンドライン引数は文字列として渡されるため、必要に応じて数値などの型変換が必要になる場合があります。

さらに、コマンドライン引数を受け取るのに便利なライブラリや、複数のコマンドライン引数をまとめて受け取る方法など、さまざまなテクニックがあります。詳細については、以下のリンクを参考にしてください。

# 参考リンク

[Microsoft Docs: コマンドライン引数に対するアクセス](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/main-and-command-args/?view=netframework-4.7.2)

[C# エキスパートプログラミング入門：コマンドライン引数の処理](https://www.atmarkit.co.jp/ait/articles/0806/05/news146.html)

[C# 超入門：コマンドライン引数を受け取る方法](https://tps-study-hirooka.hateblo.jp/entry/2020/04/21/210000)