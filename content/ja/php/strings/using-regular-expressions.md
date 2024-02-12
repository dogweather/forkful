---
title:                "正規表現の使用"
aliases:
- /ja/php/using-regular-expressions.md
date:                  2024-02-03T19:17:55.001320-07:00
model:                 gpt-4-0125-preview
simple_title:         "正規表現の使用"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ?

PHPにおける正規表現（regex）は、文字列内の文字の組み合わせをマッチさせるために使用されるパターンであり、洗練された検索・置換操作やデータ検証を可能にします。プログラマーは、テキスト解析、フォームの検証、またはウェブデータのスクレイピングなどにおいて、その力と柔軟性を活用しています。これにより、開発者のツールキットにおいて欠かせないツールとなっています。

## 使い方:

PHPはPCRE（Perlと互換性のある正規表現）ライブラリを通じて正規表現をサポートしており、豊富な関数セットを提供しています。以下がその使用方法です：

### パターンのマッチング:

文字列内にパターンが存在するかどうかを確認するには、`preg_match()`を使用します。この関数は、文字列内にパターンが見つかった場合は1を、見つからなかった場合は0を返します。

```php
if (preg_match("/\bweb\b/i", "PHPはウェブスクリプト言語です")) {
    echo "マッチが見つかりました。";
} else {
    echo "マッチが見つかりませんでした。";
}
// 出力: マッチが見つかりました。
```

### すべてのマッチを見つける:

`preg_match_all()`は、文字列内のパターンのすべての出現を見つける必要がある場合に使用されます。

```php
$text = "猫と犬";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// 出力: Array ( [0] => cats [1] => and [2] => dogs )
```

### テキストの置換:

正規表現にマッチするテキストを置き換えるには、`preg_replace()`が使用されます。これはデータのフォーマットやクリーニングに非常に強力です。

```php
$originalText = "2003年4月15日";
$pattern = "/(\w+) (\d+), (\d+)/i";
$replacement = '${1}1,$3';
echo preg_replace($pattern, $replacement, $originalText);
// 出力: 2003年4月1,15
```

### 文字列の分割:

`preg_split()`を使用して、デリミタとしてパターンを指定して文字列を配列に分割できます。

```php
$text = "PHPは、非常に人気のある、スクリプト言語です";
$parts = preg_split("/,\s*/", $text);
print_r($parts);
// 出力: Array ( [0] => PHPは [1] => 非常に人気のある [2] => スクリプト言語です )
```

さらに、複雑なregexパターンやタスクについては、Symfonyの`Finder`コンポーネントやLaravelのヘルパー関数のコレクションなどのフレームワークやライブラリがより便利な抽象化層を提供するかもしれません。しかし、PHPスクリプト内での効率的なテキスト処理や検証のためには、PHPの組み込みPCRE関数を理解して利用することが重要です。
