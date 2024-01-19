---
title:                "文字列を大文字にする"
html_title:           "Bash: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を大文字化するとは、文字列内のすべての文字を大文字に変換することを指します。これをプログラマが行う理由は、一般的にデータの一貫性を保つため、またはユーザーとのインタラクションを向上させるためです。

## 方法：

文字列を大文字に変換するバッシュコマンドには次のようなものがあります：

```Bash
input="Hello World"
echo "${input^^}"
```

上記のコードを実行すると、全てが大文字になった'HELLO WORLD'という文字列が出力されます。

## 詳細:

バッシュの大文字変換は、バージョン4から利用可能になりました。バージョン4より前では、trコマンドを使って同様のことを実現できます。しかし、バッシュの大文字変換は、位置パラメータや配列の全要素にまで適用できるため、より強力で便利です。

```Bash
input="Hello World"
echo $input | tr [:lower:] [:upper:]
```
他のオプションとして、awkやperlのような他のユーティリティを使うこともできます。

## 参考文献：

具体的な方法やより詳細な情報は以下のリンクから得ることができます。

1. Bashの公式ドキュメント：https://www.gnu.org/software/bash/manual/bash.html
2. 文字変換の詳細：https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
3. Unix/Linux tr コマンド使用法の指南：https://www.cyberciti.biz/faq/howto-use-linux-unix-tr-command/
4. AWKによる文字列の操作：https://www.gnu.org/software/gawk/manual/html_node/String-Functions.html
5. Perl の変換関数：https://perldoc.perl.org/functions/uc.html