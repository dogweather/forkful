---
title:                "Python: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読み込む理由は様々です。たとえば、データ分析を行うためにテキストファイルから情報を取得したい場合や、テキストファイルからの入力を必要とするプログラムを作成する必要がある場合などが挙げられます。Pythonを使ってテキストファイルを読み込む方法を学ぶことは、プログラム開発やデータ分析のスキルを向上させるうえで非常に役立ちます。

## How To

テキストファイルを読み込むためにPythonでどのようにコーディングするかを見ていきましょう。まずは、`open()`関数を使ってファイルを開きます。引数には、ファイル名とオープンモードを指定します。次に、`read()`メソッドを使ってファイルの中身を読み込み、変数に保存します。最後に、`close()`メソッドを使ってファイルを閉じます。

```Python
file = open("sample.txt", "r")
contents = file.read()
file.close()
```

上記の例では、`open()`関数の第二引数に`"r"`と指定しています。これはリードモードを意味し、ファイルを読み込むことができるようにするためのものです。また、`read()`メソッドはファイルの内容をすべて文字列として返します。

実際には、`with`文を使うことでファイルを開く際に`close()`メソッドを明示的に呼び出す必要がなくなります。また、`encoding`引数を使うことで、日本語などの特殊文字を正しく読み込むことができます。

```Python
with open("sample.txt", "r", encoding="utf-8") as file:
    contents = file.read()
```

では、実際にテキストファイルを読み込んで出力してみましょう。

sample.txtの中身：

```
こんにちは、世界！
Hello, world!
```

出力結果：

```
こんにちは、世界！
Hello, world!
```

## Deep Dive

上記の方法では、ファイル全体を一度に読み込んでしまいます。しかし、ファイルが非常に大きい場合や、メモリが少ない場合には問題が発生することがあります。そのような場合には、`read()`メソッドの代わりに`readline()`メソッドを使うことで、1行ずつファイルを読み込むことができます。

また、`for`ループを使ってファイルを1行ずつ処理することもできます。

```Python
with open("sample.txt", "r", encoding="utf-8") as file:
    for line in file:
        print(line)
```

さらに、`split()`メソッドを使って文字列を分割し、リストとして取得することもできます。

```Python
with open("sample.txt", "r", encoding="utf-8") as file:
    contents = file.read().split()
print(contents) # ['こんにちは、世界！', 'Hello,', 'world!']
```

テキストファイルの操作については、さまざまな方法がありますので、ぜひ実際に試してみてください。

## See Also

- [Python公式ドキュメント - Files](https://docs.python.org/ja/3/tutorial/inputoutput.html#