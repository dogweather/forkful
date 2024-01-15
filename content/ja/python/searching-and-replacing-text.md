---
title:                "テキストの検索と置換"
html_title:           "Python: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

テキストを検索して置換するメソッドは、プログラマーにとって非常に便利です。例えば、大量のコードやテキストファイルを一括で変更することができるため、作業の効率を大幅に上げることができます。

## How To

検索して置換を行うためには、Pythonで提供されている`replace()`メソッドを使用します。このメソッドは、指定した文字列を検索して、別の文字列に置換することができます。以下は、実際に`replace()`メソッドを使用したコードの例です。

```Python
# 文字列を定義する
text = "こんにちは！私の名前はコウです。"

# "コウ"を"太郎"に置換する
new_text = text.replace("コウ", "太郎")

# 結果をプリントする
print(new_text)
```

出力結果：
```
こんにちは！私の名前は太郎です。
```

`replace()`メソッドでは、検索文字列だけでなく、置換する回数を指定することもできます。

```Python
# 文字列を定義する
text = "Pythonはとても素晴らしい言語です。Pythonはとても人気があります。"

# "Python"を"Java"に置換する（最初の1回のみ）
new_text = text.replace("Python", "Java", 1)

# 結果をプリントする
print(new_text)
```

出力結果：
```
Javaはとても素晴らしい言語です。Pythonはとても人気があります。
```

## Deep Dive

`replace()`メソッドは、完全一致した文字列のみを置換するため、正確な検索が必要です。正規表現を使用することで、より柔軟な検索と置換が可能です。Pythonでは、正規表現を扱うための`re`モジュールが用意されています。

以下は、`re`モジュールを使用して正規表現を用いた検索と置換の例です。

```Python
# reモジュールをインポートする
import re

# 文字列を定義する
text = "今日はとても暑いです。今日の最高気温は35度です。"

# 正規表現を用いて、最高気温をXX度に置換する
new_text = re.sub(r"([0-9]+)度", "XX度", text)

# 結果をプリントする
print(new_text)
```

出力結果：
```
今日はとても暑いです。今日の最高気温はXX度です。
```

See Also

- [Pythonのreモジュールドキュメント](https://docs.python.org/ja/3/library/re.html)
- [Pythonで正規表現を使いこなす](https://qiita.com/tekamaru/items/5d424086b29cfcd21ac8)
- [正規表現チートシート](http://www.nextzy.me/wp-content/uploads/2016/03/python-cheatsheetregular-expression-1.png)