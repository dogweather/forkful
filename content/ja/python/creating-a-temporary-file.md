---
title:                "Python: 「一時ファイルの作成」"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時的なファイルを作成することに取り組む理由は様々ですが、最も一般的な理由は、プログラムの実行に必要な一時的なデータやキャッシュを保存することです。また、一時的なファイルは必要な時にのみ作成されるため、プログラムの実行中に不要なファイルが残る心配がありません。

## 作り方

一時的なファイルを作成するためには、Pythonの組み込み関数である```tempfile```を使用します。まずは、このモジュールをインポートします。

```Python
import tempfile
```

次に、一時的なファイルを作成するための準備をします。今回は、一時的なテキストファイルを作成する例を示します。

```Python
temp_file = tempfile.NamedTemporaryFile(mode="w+t")
```

```mode```パラメーターでは、作成されるファイルのモードを指定することができます。ここでは、```w+t```を指定しています。これは、テキストファイルを書き込み用と読み込み用の両方のモードで開くという意味です。

ここで、作成した一時的なファイルにデータを書き込みます。以下の例では、```write```メソッドを使用して、ファイルに文字列を書き込んでいます。

```Python
temp_file.write("こんにちは、一時的なファイル！")
```

そして、最後に、イテレーターを使用してファイルの内容を読み取ります。

```Python
temp_file.seek(0)
print(temp_file.read())
```

出力結果は以下の通りになります。

```Python
こんにちは、一時的なファイル！
```

一時的なファイルはプログラムが終了すると自動的に削除されるため、不要なファイルが残る心配はありません。

## 深く掘り下げる

実際には、一時的なファイルを作成する際にはもっと複雑な用途があります。例えば、一時的なディレクトリを作成したり、一時的なファイルを圧縮したりすることもできます。また、```tempfile```モジュールにはさまざまなパラメーターが用意されており、作成する一時的なファイルの種類や動作をカスタマイズすることができます。詳細な情報は、Python公式ドキュメントを参照してください。

## 参考リンク

- [Python公式ドキュメント - tempfileモジュール](https://docs.python.org/ja/3/library/tempfile.html)
- [Python公式ドキュメント - ファイルの扱い方](https://docs.python.org/ja/3/tutorial/inputoutput.html)