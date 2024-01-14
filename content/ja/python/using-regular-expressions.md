---
title:    "Python: 正規表現の使用"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ
正規表現を使用するのには、テキストから特定のパターンや文字列を抽出する必要がある場合に非常に役立ちます。例えば、特定の形式を持つメールアドレスや電話番号を入力フォームから抽出したい場合などです。

## 使い方
正規表現を使用するには、Pythonのreモジュールをインポートする必要があります。以下は、電話番号を抽出するコードの例です。

```Python
import re

text = "私の電話番号は080-1111-2222です。"
pattern = r"\d{3}-\d{4}-\d{4}" # 電話番号の正規表現
match = re.search(pattern, text) # 正規表現をテキストに適用
print(match[0]) # テキストから抽出した電話番号を表示
```
以下は、上記コードの出力例です。

```
080-1111-2222
```

## 深堀り
正規表現のパターンの書き方には、様々な記号やワイルドカードがあります。例えば、`\d`は数字を表し、`\w`は文字（アルファベットや数字）を表します。また、`{}`を使用することで、その前の文字やワイルドカードの出現回数を指定することができます。詳しくは、正規表現のチートシートやチュートリアルを参照してください。

## 他に見るべきもの
- [Pythonの公式reモジュールドキュメント](https://docs.python.org/ja/3/library/re.html#module-re)
- [正規表現のチートシート](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [正規表現のチュートリアル](https://www.w3schools.com/python/python_regex.asp)

# 参考