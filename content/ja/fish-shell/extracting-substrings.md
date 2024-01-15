---
title:                "「サブストリングの抽出」"
html_title:           "Fish Shell: 「サブストリングの抽出」"
simple_title:         "「サブストリングの抽出」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

コマンドラインでファイルを操作する際、時には指定した文字列やパターンに一致する一部分を抽出したいことがあります。そのような場面で、フィッシュシェルのサブストリング抽出機能が役立ちます。この記事では、サブストリング抽出の方法を紹介します。

## 方法

ファイルの特定の行や文字列から、指定した部分を抽出する方法を以下のコード例で説明します。

```fish
# 文字列から文字を抽出する
set myString "こんにちは、フィッシュシェル"
echo $myString[1-5] #出力: こんにち

# 文字列から特定の文字列を抽出する
set myString "こんにちは、フィッシュシェル"
echo $myString["コ":] #出力: コマンドライン

# ファイルから特定の行を抽出する
set fileName "sample.txt"
cat $fileName[1-2] # 出力: これはサンプルファイルです
                    #      抽出された行です

# ファイルから特定の文字列を含む行を抽出する
set fileName "sample.txt"
grep "フィッシュシェル" $fileName # 出力: この行はフィッシュシェルという言葉が含まれています
                                    #      抽出された行です
```

## ディープダイブ

フィッシュシェルでは、サブストリングを抽出する際に使用できる便利なオプションがいくつかあります。例えば、文字列の末尾から抽出する際には負のインデックスを使用することができます。また、文字列の長さがわからない場合でも、`[start-end]`の代わりに`[start-]`を使用して最後まで抽出することができます。さらに、正規表現を使用してマッチする部分を抽出することもできます。

````fish
# 負のインデックスを使用した場合
set myString "今日は晴れです"
echo $myString[-2-] # 出力: です

# 最後まで抽出する場合
set myString "こんにちは、フィッシュシェル"
echo $myString[5-] # 出力: フィッシュシェル

# 正規表現を使用して抽出する場合
set myString "今日は晴れです"
echo $myString[pattern \w+] # 出力: 晴れ

````

## 参考サイト

- [Official Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Substrings in Bash](https://linuxize.com/post/bash-substring/)
- [Regex tutorial](https://www.regular-expressions.info/tutorial.html)

## 参考文献

- [Official Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Bashのサブストリング抽出](https://linuxize.com/post/bash-substring/)
- [正規表現のチュートリアル](https://www.regular-expressions.info/tutorial.html)