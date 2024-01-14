---
title:    "Python: テキストの検索と置換"
keywords: ["Python"]
---

{{< edit_this_page >}}

## なぜ検索・置換テキストを使うのか？

検索・置換テキストは、大量のテキストデータを効率的に編集するために便利なツールです。例えば、大規模な文書内で特定の単語を一括で変更する場合や、複数のファイル内で同じ変数名を一括で修正する場合などに活用されます。手作業で1つ1つ変更するよりも、簡単かつ迅速に作業を行うことができます。

## 使い方

```Python
# 文字列内の特定の単語の置換
text = "Pythonは素晴らしいプログラミング言語です。"
new_text = text.replace("Python", "Java")
print(new_text) # 出力結果：Javaは素晴らしいプログラミング言語です。

# ファイル内の特定の文字列の置換
file = open("sample.txt", "r")
new_file = open("new_sample.txt", "w")

for line in file:
    new_line = line.replace("Hello", "こんにちは")
    new_file.write(new_line)

file.close()
new_file.close()
```

上記のPythonコードでは、`replace()`メソッドを使って文字列やファイルの中の特定の単語や文字列を置換しています。文字列内の単語の検索と置換は`replace()`メソッドを使用し、ファイル内の検索と置換はループ処理を使用して行います。

## さらに掘り下げる

検索・置換テキストの実装には、正規表現を使用する方法もあります。正規表現を使用すると、より柔軟な検索や置換が可能になります。また、Pythonの`re`モジュールを使用することで、正規表現をより効率的に扱うことができます。正規表現の詳細については、Pythonの公式ドキュメントやオンラインのチュートリアルを参考にしてください。

## 参考リンク

- [Python公式ドキュメント](https://docs.python.org/ja/3/library/re.html)
- [正規表現チュートリアル](https://techacademy.jp/magazine/19430)
- [Pythonで文字列を置換する方法](https://note.nkmk.me/python-str-replace/)

## 参考資料

- [Python公式ドキュメント：文字列を検索する](https://docs.python.org/ja/3/library/stdtypes.html#string-methods)
- [Python公式ドキュメント：ファイル入出力](https://docs.python.org/ja/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Python公式ドキュメント：正規表現操作](https://docs.python.org/ja/3/library/re.html#regular-expression-syntax)