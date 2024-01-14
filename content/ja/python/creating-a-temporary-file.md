---
title:    "Python: 一時ファイルの作成"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時的なファイルを作成する理由はさまざまです。たとえば、長期間保存する必要のないデータを一時的に保存したり、プログラムが終了すると自動的に削除されるようにしたい場合に使用することができます。

## 作り方

Pythonでは、一時的なファイルを作成するために、`tempfile`モジュールを使用します。以下の例では、ファイルパス、内容、エンコーディングを指定して一時的なテキストファイルを作成し、書き込みを行い、最後にファイルを閉じます。

```Python
import tempfile

# 一時的なテキストファイルを作成
with tempfile.TemporaryFile(mode='w+t', encoding='utf-8') as f:
    # ファイルに書き込み
    f.write('この文章は一時的なファイルに書き込まれます。')

    # ファイルの先頭に戻る
    f.seek(0)

    # ファイルの内容を読み取り
    print(f.read())
```

上記のコードを実行すると、以下のようにファイルの内容が出力されます。

```
この文章は一時的なファイルに書き込まれます。
```

## 深堀り

一時的なファイルを作成する際には、ファイルの作成場所や名前の衝突を避けることが重要です。そのために、`tempfile`モジュールは一時的なファイルを保存するディレクトリを自動的に選択します。また、`NamedTemporaryFile`を使用するとファイル名を指定することもできます。

一時的なファイルを作成する際には、ファイルの自動削除についても注意が必要です。一時的なファイルは、プログラムが終了すると自動的に削除されるようになっていますが、途中でプログラムが終了してしまった場合や、ファイルを閉じ忘れた場合は手動で削除する必要があります。

## さらに参考になる情報

- Python公式ドキュメント: [tempfile — 一時ファイルやディレクトリのユーティリティ](https://docs.python.org/ja/3/library/tempfile.html)
- Qiita: [Pythonのtempfileモジュールを使って一時的なファイルを作成する方法](https://qiita.com/jetbead/items/890cd6f1a832206d37c6)
- Pythonプログラミング入門: [一時ファイルの使用](https://python.programming-guide.jp/part11/temporary-file/)