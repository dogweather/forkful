---
title:                "正規表現を使う"
html_title:           "Python: 正規表現を使う"
simple_title:         "正規表現を使う"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

＃＃　なぜ
正規表現を使う理由を最大２文で説明します。

正規表現は、特定のパターンを検索や抽出するための強力なツールです。例えば、長いテキストの中からメールアドレスや電話番号を見つける、あるいは特定の単語を含む文章をすべて置換するなどの作業が簡単にできます。正規表現を使うことで、よりスマートかつ効率的なコーディングが可能になります。

＃＃　使い方
正規表現を使うための具体的なコーディング例と、出力サンプルを示します。コードはすべてPythonの```...```コードブロックで示しています。

### テキスト中からメールアドレスを抽出する例

```Python
import re

text = "私のメールアドレスはtest@gmail.comです。お問い合わせはこちらにお願いします。"
pattern = r"\w+@\w+\.com"
matches = re.findall(pattern, text)
print(matches)

# Output: ['test@gmail.com']
```

### 文章内の単語を置換する例

```Python
import re

text = "私はとても楽しいです。私は本当に嬉しいです。"
pattern = r"楽しい"
new_text = re.sub(pattern, "素敵", text)
print(new_text)

# Output: 私はとても素敵です。私は本当に嬉しいです。
```

### 正規表現の基本パターン

Pythonでよく使われる正規表現の基本パターンを以下にまとめます。

| パターン | 説明 |
| ------ | ------ |
| \w | 数字やアルファベット、アンダースコアにマッチ |
| \d | 数字にマッチ |
| \s | 空白文字にマッチ |
| ^ | 文字列の先頭にマッチ |
| $ | 文字列の末尾にマッチ |
| [...] | 指定した文字列のいずれかにマッチ |
| [^...] | 指定した文字列以外のいずれかにマッチ |

＃＃　詳細情報
正規表現にはさまざまなオプションや特殊文字があり、さらに高度なパターンマッチングが可能です。詳しい情報は公式ドキュメントを参照するか、オンライン上で利用可能なチュートリアルを参考にしてください。

例えば、正規表現では「グループ化」と呼ばれる機能を使うことで、マッチした部分だけを取り出すことができます。また、```re.compile()```を使うことで、複数のパターンを一度に検索することもできます。

＃＃　参考情報
「See Also」
- Python公式ドキュメント: https://docs.python.org/ja/3/library/re.html
- 正規表現チュートリアル: https://www.programiz.com/python-programming/regex