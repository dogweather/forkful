---
title:                "正規表現の使用"
date:                  2024-01-19
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
正規表現とは？プログラマーがなぜ使うのか？

正規表現（RegExp）は文字列内でのパターン検索や置き換えに使用する強力なツールです。テキストデータを効率的に処理するため、パターンマッチングやテキスト操作などを柔軟に行うために使われます。

## How to:
### 文字列内のパターン検索
```Fish Shell
echo "今日は2023年3月15日です" | string match -r '[0-9]+年[0-9]+月[0-9]+日'
```
#### 出力:
```
2023年3月15日
```

### 文字列置換
```Fish Shell
echo "FishShellは素晴らしい!" | string replace -r '素晴らしい' '強力な'
```
#### 出力:
```
FishShellは強力な!
```

## Deep Dive
### 歴史的背景
正規表現は、1950年代に数学者スティーブン・クリーネによって提案されました。プログラミング言語Perlが普及すると共に広く使われるようになりました。Fish Shellでは`string`コマンドを利用して正規表現が実装されています。

### 代替手段
正規表現以外にも`grep`、`sed`、`awk`のようなツールでテキスト処理が可能ですが、Fish Shell内蔵コマンド`string`の使用は簡単で統合性があります。

### 実装詳細
Fish Shellの正規表現は、POSIX準拠のエンジンを使用し、基本的なパターンマッチングからより複雑な検索・置き換えまで実現できます。フラグや構文は他のツールとは異なる場合があるため、`man string`を参照しましょう。

## See Also
- Fish公式ドキュメント: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- 正規表現についての詳細: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
- オンライン正規表現テスター: [https://regex101.com/](https://regex101.com/)
