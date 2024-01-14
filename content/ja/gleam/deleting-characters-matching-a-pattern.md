---
title:    "Gleam: パターンに一致する文字を削除する"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# なぜ

この記事では、Gleamプログラミング言語でパターンにマッチする文字を削除する方法について説明します。パターンにマッチする文字を削除することで、プログラムをより効率的に処理し、コードの動作を改善することができます。

# 方法

まず、`String.replace`関数を使用して、パターンにマッチする文字を空文字列に置き換えます。

```
Gleam string.replace("Hello World", "o", "")
```

このコードの出力は`Hell Wrld`となります。パターンにマッチする文字がない場合は、元の文字列がそのまま返されます。

# 深堀り

この方法でパターンにマッチする文字を削除するには、正規表現を使用します。Gleamでは、`Regex.replace`関数を使用してパターンにマッチする文字を置き換えることができます。

```
Gleam regex.replace("Hello World!", "[aeiou]", "")
```

このコードの出力は`Hll Wrld!`となり、母音を削除することができます。また、正規表現を使用することで、より複雑なパターンにもマッチすることができます。

# 参考リンク

- [Gleamの公式ドキュメント](https://gleam.run/documentation/?api=string#replace)
- [正規表現の基礎](https://qiita.com/kewpie134134/private/6a848b7ce60f5c7b2d87)