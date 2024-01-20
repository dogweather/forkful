---
title:                "正規表現の使用"
html_title:           "Bash: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何となぜ？
正規表現は、文字列のパターンを検索や置換するためのパワフルなツールです。プログラマは、複雑なテキスト処理を考える代わりに、高速でスケーラブルなソリューションを得るためにこれを使用します。

## 使用方法：
正規表現を使った基本的な文字列の検索を見てみましょう。

```Bash
echo "Hello, Coders of Japan" | grep -o "Hello"
```

出力：

```Bash
Hello
```

grepと正規表現を使用して特定のパターンをマッチングする方法もあります。たとえば、aからzまでの小文字アルファベットを検索したい場合は、以下のようにします。

```Bash
echo "Hello, Coders of Japan" | grep -o "[a-z]*"
```

出力：

```Bash
ello
oders
of
apan
```

## ディープダイブ：
正規表現は、初めて1950年代に作られ、数十年にわたり、その能力と柔軟性を増してきました。Bashでの実装はPCRE(Perl Compatible Regular Expressions)に似ており、いくつかの特性と利点があります。

代替案としては、awkやsedのようなテキスト処理ツールがありますが、大規模なテキスト処理においては、正規表現が一番パフォーマンスが高いです。

## 参考文献：
1. [正規表現チュートリアル](https://www.regular-expressions.info/tutorial.html)
2. [Bashでの正規表現の詳細](https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html)
3. [正規表現とは何か](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions)