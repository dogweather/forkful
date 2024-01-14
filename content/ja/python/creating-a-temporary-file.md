---
title:                "Python: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why
ファイルを一時的に作成する必要がある理由はたくさんあります。例えば、プログラムの実行中に一時的にデータを保存する必要がある場合や、セキュリティ上の理由で必要なときだけファイルを使う場合などです。

## How To
一時的なファイルを作成するには、Pythonの`tempfile`モジュールを使って以下のようにコードを書くことができます。

```Python
import tempfile

# 一時的なディレクトリを作成する
temp_dir = tempfile.mkdtemp()
print(temp_dir)

# 一時的なファイルを作成する
temp_file = tempfile.NamedTemporaryFile()
print(temp_file.name)

# テキストファイルを一時的に作成し、書き込む
with tempfile.NamedTemporaryFile(mode='w+t', delete=False) as temp_text:
    temp_text.write("Hello World!")
    print(temp_text.name)
```

上記のコードを実行すると、一時的なディレクトリとファイルが作成され、そのパスが出力されます。デフォルトでは、プログラムが終了すると一時的なファイルやディレクトリは自動的に削除されますが、`delete=False`を指定することで手動で削除することもできます。

## Deep Dive
Pythonの`tempfile`モジュールには、一時的なファイルやディレクトリをより精密に扱うためのさまざまな関数やオプションが用意されています。例えば、一時的なファイルを作成する際にテキストエディターを開くこともできます。また、一時的なファイルを保存する場所やファイル名、拡張子を指定することもできます。

ファイルの扱いによっては、`tempfile`モジュールだけでは不十分な場合もあります。その場合は、Pythonの`os`モジュールを組み合わせて使うことでより柔軟に一時的なファイルを作成できます。

## See Also
- [Pythonの`tempfile`モジュールのドキュメント](https://docs.python.org/ja/3/library/tempfile.html)
- [Pythonの`os`モジュールのドキュメント](https://docs.python.org/ja/3/library/os.html)