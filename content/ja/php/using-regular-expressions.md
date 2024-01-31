---
title:                "正規表現の使用"
date:                  2024-01-19
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
正規表現は、文字列内でパターンを探す手段だ。プログラマーはコードをスリムにし、テキストデータを効率的に解析、変換するために使う。

## How to: (方法)
```PHP
<?php
$subject = "今日は2023年3月15日です。";
$pattern = '/\d{4}年\d{1,2}月\d{1,2}日/';

// マッチする文字列を探す
if (preg_match($pattern, $subject, $matches)) {
    echo "見つかった日付: " . $matches[0]; // 出力: 見つかった日付: 2023年3月15日
} else {
    echo "日付は見つかりませんでした。";
}

// 文字列置換
$replacement = '[日付]';
$newString = preg_replace($pattern, $replacement, $subject);
echo "\n置換後の文字列: " . $newString; // 出力: 置換後の文字列: 今日は[日付]です。
?>
```

## Deep Dive (深み)
正規表現は、1960年代に発展した。実用性の高いパターンマッチングツールとして拡大し、今や多くのプログラミング言語で標準機能となっている。PHPでは`preg`関数ファミリー（PCRE - Perl Compatible Regular Expressions を使用）を用いる。`preg_match`はマッチするかテストする時、`preg_replace`は置換に。`strstr`や`strpos`といった単純な文字検索関数もあるけど、パターンマッチングの柔軟性には劣る。

## See Also (関連情報)
- [PHP: preg_match - Manual](https://www.php.net/manual/function.preg-match.php)
- [PHP: preg_replace - Manual](https://www.php.net/manual/function.preg-replace.php)
- [PHP Regular Expressions (PHP: PCRE) - Manual](https://www.php.net/manual/book.pcre.php)
