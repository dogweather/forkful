---
title:    "Python: 一時ファイルの作成"
keywords: ["Python"]
---

{{< edit_this_page >}}

## なぜ

Pythonプログラマーは一時ファイルを作成するのか？一時ファイルはプログラムの実行中に一時的なデータを作成するために使用されます。例えば、大きなデータを処理するプログラムでは、一時ファイルを作成してそのデータを処理することができます。また、一時ファイルの使用はパフォーマンスを向上させることができるため、プログラムの実行をスムーズにすることができます。

## 作り方

Pythonでは、標準ライブラリの```tempfile```モジュールを使用して一時ファイルを作成することができます。以下のコードは、一時フォルダ内に新しい一時ファイルを作成し、そのファイルにデータを書き込んで読み取る方法を示しています。

```Python
import tempfile
# 一時フォルダを作成
temp_folder = tempfile.TemporaryDirectory()
# 一時ファイルを作成
temp_file = tempfile.NamedTemporaryFile(dir=temp_folder.name, delete=False)
# ファイルにデータを書き込む
temp_file.write(b"Hello World!")
# ファイルを閉じる
temp_file.close()
# データの読み取り
with open(temp_file.name, 'r') as f:
    data = f.read()
print(data) # 出力: Hello World!
# 一時フォルダを閉じる
temp_folder.cleanup()
```

このコードでは、```tempfile```モジュールから```TemporaryDirectory()```と```NamedTemporaryFile()```を使用していることに注意してください。また、一時ファイルを作成する際に、```delete=False```を指定することで、一時ファイルが使用後に自動的に削除されないようにすることができます。

## 深堀り

一時ファイルを作成する際には、注意することがいくつかあります。まず、一時ファイルはプログラムの実行中に一時的に使用するためのものであり、データを永続的に保存する際には使用しないようにしましょう。また、一時ファイルはプライバシーやセキュリティに関する問題を引き起こす可能性があるため、注意深く扱う必要があります。最後に、一時ファイルの作成にはディスクアクセスが必要となるため、プログラムの実行速度を考慮して適切なタイミングで作成するようにしましょう。

## 関連情報を見る

* [Python公式ドキュメント - tempfileモジュール](https://docs.python.org/ja/3/library/tempfile.html)
* [Python Tips - 一時ファイルの作成方法と使い方](https://note.nkmk.me/python-tempfile/)