---
title:                "Python: テキストの検索と置換"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

あなたがPythonプログラミングを始めたばかりであろうか？または、プログラミングの新しいトピックを学びたいと思っているかもしれません。今回は、テキストの検索と置換の方法についてお話します。テキストの検索と置換は、よく使用されるテキスト処理の技術です。プログラマーであれば、必ずこの技術を学ぶ必要があります。それでは、Pythonを使用してテキストの検索と置換をどのように行うか見ていきましょう。

## 使い方

Pythonでテキストの検索と置換を行うには、まず文字列メソッドの `replace()` を使用します。このメソッドは、文字列内で指定した部分文字列を別の文字列に置換することができます。例えば、次のようにコーディングすることができます。

```Python
# テキストを入力
text = "こんにちは、Pythonを学ぼう！"

# 検索する文字列と置換する文字列を指定
search = "Python"
replace = "プログラミング"

# 文字列を置換
new_text = text.replace(search, replace)

# 結果を出力
print(new_text)

# 出力：こんにちは、プログラミングを学ぼう！
```
上記のコードでは、まず `replace()` メソッドを使用して `Python` という文字列を `プログラミング` に置換し、その結果を `print()` メソッドで出力しています。

## 詳しく調べる

テキストの検索と置換を行うだけではなく、Pythonではさまざまな検索パターンを指定することもできます。例えば、正規表現を使用して複雑なパターンを指定することができます。また、複数の文字列を一度に置換することも可能です。

さらに、Pythonには `re` というモジュールがあり、正規表現をより便利に使用することができます。このモジュールでは、検索結果の文字列の取得やパターンの置換などを行うことができます。

もしあなたがPythonでより高度なテキスト処理を行いたいのであれば、正規表現や `re` モジュールを学ぶことをお勧めします。

## もっと詳しく知るには

- [Python 公式ドキュメント：文字列メソッド](https://docs.python.org/ja/3/library/stdtypes.html#string-methods)
- [Python 公式ドキュメント：正規表現 HOWTO](https://docs.python.org/ja/3/howto/regex.html)
- [正規表現入門](https://qiita.com/gmkou/items/3458c30808c6a19e02fc)
- [Python チュートリアル：正規表現を使用したパターンマッチング](https://docs.python.org/ja/3/tutorial/stdlib2.html#pattern-matching)
- [Python チュートリアル：正規表現を使用した文字列の操作方法](https://docs.python.org/ja/3/tutorial/stdlib2.html#string-methods-on-regular-expressions)

## 関連リンク

- [Markdown について知る](https://qiita.com/satokaz/items/72bf914dba447aa2de8b)
- [Markdown エディタのおす