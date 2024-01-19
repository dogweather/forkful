---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何となぜ？ 

テキストの検索・置換は、特定の文字列を探したり、新しい文字列に置き換えたりする処理のことです。プログラマーは、データの修正、コードのリファクタリング、情報の抽出などの目的でこれを行います。

## どのように: 

Pythonでの基本的な検索と置換は、`str.replace()`関数を使用して行います。具体的なコード例は以下の通りです：

```Python
# 文字列定義
s = "Hello, World!"
print("Original String: ", s)

# 'World' を 'Japan' へ置換
new_s = s.replace("World", "Japan")
print("Updated String: ", new_s)
```

実行結果：

```Python
Original String:  Hello, World!
Updated String:  Hello, Japan!
```

## ディープダイブ: 

検索と置換の技術は古くからあり、さまざまなプログラミング言語で実装されています。Pythonだけでなく、Java, C#, Perl等でも同様の操作が可能です。

また、正規表現を使用すると、より複雑なパターンのテキストを検索・置換することも可能です。Pythonの`re`モジュールは正規表現を扱うための機能を提供しています。

一方、文字列の検索・置換の内部の動きは、Pythonの`str`クラス内のメソッドで処理されています。テキスト全体をスキャンして検索文字列を見つけ、新しい文字列で置き換えて最終的な文字列を作っています。

## 参照: 

以下はテキスト検索・置換に関して学ぶことが出来る追加の資料のリンク集です。

- Python公式ドキュメンテーション: `str.replace()関数`: [Python Documentation](https://docs.python.org/3/library/stdtypes.html#str.replace)
- Python公式ドキュメンテーション: `reモジュール`: [Python Documentation](https://docs.python.org/3/library/re.html)
- 正規表現チュートリアル: [RegEx Tutorial](https://www.w3schools.com/python/python_regex.asp)