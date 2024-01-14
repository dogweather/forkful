---
title:                "Fish Shell: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ 

文字列を小文字に変換する必要があるのかを1-2文で説明します。

例えば、日本語のコードで検索する際には平仮名とカタカナの区別をしないようにするために、大文字と小文字の区別がなくなる文字列に変換することが重要です。

## 方法 

Fish Shellを使用して文字列を小文字に変換する方法を示します。

```
# 変換前の文字列
set message "こんにちは！"

# 変換後の文字列
set lower_message (string tolower $message)

# 変換結果の出力
echo $lower_message

# output:
# こんにちは！
```

## 深く掘り下げる 

文字列を小文字に変換する際、Fish Shellはどのように処理を行っているのでしょうか。

まず、`string tolower`コマンドは引数として受け取った文字列をすべて小文字に変換します。そして、変換した文字列を`set`コマンドを用いて新しい変数に格納します。これにより、変換前の文字列と変換後の文字列を比較することができます。

また、日本語のコードで使用されるひらがなやカタカナは小文字と大文字という概念がないため、`string tolower`コマンドを使用しても変化はありません。しかし、このような場合でも一貫性を保つために変換することは重要です。

## See Also 

- [Fish Shell 公式サイト](https://fishshell.com/)
- [Fish Shell ドキュメント](https://fishshell.com/docs/current/)
- [Fish Shell における文字列操作](https://fishshell.com/docs/current/cmds/string.html)