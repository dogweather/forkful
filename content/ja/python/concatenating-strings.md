---
title:                "文字列の連結"
html_title:           "Python: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の結合をする理由は様々です。例えば、複数の変数を組み合わせて新しい文を作り出すことができたり、フォーマットされた文を作ることができます。文字列の結合は、プログラムでよく使われる基本的な処理であるため、覚えておくと便利です。

## 方法

文字列の結合には、いくつかの方法があります。まずは、単純なものから見ていきましょう。

```Python
first_name = "太郎"
last_name = "山田"
full_name = first_name + last_name
print(full_name)
```
出力結果: 太郎山田

2つの変数を結合するには、`+`演算子を使用します。この方法は、コードが簡単で読みやすくなりますが、大量の文字列を結合する場合は、効率が悪いかもしれません。

次に、`join()`メソッドを使用する方法を紹介します。

```Python
fruits = ["りんご", "バナナ", "オレンジ"]
fruits_string = ", ".join(fruits)
print(fruits_string)
```
出力結果: りんご, バナナ, オレンジ

ここでは、リストの各要素を`join()`メソッドを使って結合し、区切り文字としてカンマとスペースを指定しています。この方法は、複数の文字列を1つの文字列にまとめる場合に便利です。

さらに、文字列のフォーマットを行う際には、`format()`メソッドを使用することもできます。

```Python
name = "鈴木"
age = 30
print("私の名前は{}です。年齢は{}歳です。".format(name, age))
```
出力結果: 私の名前は鈴木です。年齢は30歳です。

`{}`の中にインデックス番号を指定することで、複数の変数を順番に結合することができます。

## ディープダイブ

文字列の結合は、実は文字列の操作では比較的重い処理です。なぜなら、文字列は不変（immutable）のデータ型であり、結合するたびに新しい文字列が作られるからです。そのため、大量の文字列を結合する処理を行う際には、メモリの使用量に注意が必要です。

また、文字列の結合はパフォーマンスの面でも影響を与えることがあります。`+`演算子や`join()`メソッドは、内部で新しいオブジェクトを作成するため、ループ処理などで頻繁に使用するとパフォーマンスが落ちることがあります。そのため、大量の文字列を結合する場合には、他の方法を検討することも重要です。

## See Also

- [Python公式ドキュメント（英語）](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [文法エラーエラー：Pythonで文字列を結合する方法（日本語）](https://www.yoheim.net/blog.php?q=20150801)
- [Pythonの文字列を効率よく結合する方法（日本語）](http://nekoyukimmm.hatenablog.com/entry/2017/04/23/161051)