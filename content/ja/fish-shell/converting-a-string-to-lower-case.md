---
title:    "Fish Shell: 「文字列を小文字に変換する」"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換することについて、なぜ取り組む必要があるのか、1-2文で説明します。

## 方法

```Fish Shell```を使ったコーディング例と出力のサンプルを、コードブロックで示します。

```fish
# 文字列を小文字に変換する方法
set str "FISH SHELL"
echo "変換前の文字列: $str"
set lower (string tolower $str)
echo "変換後の文字列: $lower"
```

出力:

```
変換前の文字列: FISH SHELL
変換後の文字列: fish shell
```

## ディープダイブ

文字列を小文字に変換する上で、深く掘り下げた情報を紹介します。この方法は、文字列処理やデータの整形に使われることが多く、特定の文字の大文字小文字を無視して処理する際にも便利です。

例えば、```fish```という文字列を検索する場合、以下のように大文字小文字を無視して処理することができます。

```fish
# 大文字小文字を無視して検索する方法
set str "FISH SHELL"
set search "fish"
if string contains -i $str $search
    echo "$str には $search が含まれています。"
end
```

出力:

```
FISH SHELL には fish が含まれています。
```

このように、文字列を小文字に変換することで、柔軟に文字列の処理ができるようになります。

## 参考リンク

- [Fish Shell - The friendly interactive shell](https://fishshell.com/)
- [Fish Shell Tutorial](https://www.hostinger.com/tutorials/fish-shell)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [String manipulation in Fish Shell](https://stackoverflow.com/questions/19482125/string-manipulation-in-fish-shell)
- [Fish Shell Tips and Tricks](https://medium.com/@weblate/fish-shell-tips-and-tricks-4662fa441f9e)

## 参考になるリンク

[さらに読み込み]