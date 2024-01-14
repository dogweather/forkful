---
title:    "Fish Shell: 小文字に文字列を変換する"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# なぜ
文字列を小文字に変換する理由を説明するのは、テキストデータを正規化するためで、検索機能を改善する効果があります。

## 使い方
```Fish Shell
set str "こんにちは、Fish Shell！"
echo $str
# 出力: こんにちは、Fish Shell！

echo (string lower $str)
# 出力: こんにちは、fish shell！
```

## 深堀り
文字列を小文字に変換するメソッドには様々なオプションがあります。例えば、`string lower`コマンドを使用すると、デフォルトではユニコード文字の小文字化を行いますが、オプションでASCIIのみに限定することもできます。また、`string lowercase`コマンドを使用すると、ロケールに応じた小文字化が可能です。

# 参考リンク
## 関連記事
- [Fish Shell documentation](https://fishshell.com/docs/current/commands.html#string-case-conversion)
- [文字列操作の基本技術事典](http://www.webmaster-tips.net/Techniques/string.html)