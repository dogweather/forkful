---
title:    "Python: 正規表現を使う"
keywords: ["Python"]
---

{{< edit_this_page >}}

## なぜ正規表現を使うのか

正規表現は、テキストデータから特定のパターンを検索・抽出するために使用されます。例えば、メールアドレスや電話番号などの特定の形式で書かれているテキストを一括検索する場合、正規表現は非常に便利です。また、大量のデータから必要な情報だけを抽出する際にも役立ちます。

## 正規表現の使い方

Pythonでは、```re```モジュールを使って正規表現を処理することができます。まずは、```import re```と記述してモジュールをインポートします。次に、文字列に対して正規表現を適用する際は、```re.search()```や```re.findall()```といったメソッドを使用します。

例えば、ある英文の中からすべての電話番号を抽出したい場合は、以下のようなコードを書くことができます。

```Python
import re

text = "私の電話番号は080-1234-5678です。彼の番号は090-9876-5432です。"

phone_numbers = re.findall(r'\d{3}-\d{4}-\d{4}', text)

print(phone_numbers)
```

上記のコードでは、まず```text```という変数に対して正規表現を適用することを宣言しています。そして、```findall()```メソッドを用いて、すべての電話番号を抽出し、```phone_numbers```という変数に格納しています。最後に、抽出された電話番号を出力しています。

実行すると、次のような結果が得られます。

```
['080-1234-5678', '090-9876-5432']
```

## 正規表現の詳細

正規表現には様々なパターンがあり、特定の文字列を検索するだけでなく、置換やパターンマッチングなどにも使用することができます。正規表現の記法やメタ文字の使用方法など、詳細な情報は以下のリンクを参考にしてください。

## さらに見る

- [Pythonで正規表現を扱う - Qiita](https://qiita.com/hoto17296/items/896f994016dd3fe9c00d)
- [正規表現 HOWTO - Python公式ドキュメント](https://docs.python.org/ja/3/howto/regex.html)
- [正規表現入門 - Pythonプログラミング入門](https://python.keicode.com/text/regex-intro.php)