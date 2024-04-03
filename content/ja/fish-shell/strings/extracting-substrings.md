---
date: 2024-01-20 17:45:47.874348-07:00
description: "How to (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.715982-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## How to (方法)
```Fish Shell
# サブストリング抽出の例:
set my_string "Fish Shell is fun!"
# 6文字目から10文字目を取り出す
echo $my_string | string sub -s 6 -l 5
```
出力: `Shell`

```Fish Shell
# 文字列の先頭から4文字を取り出す
echo $my_string | string sub -l 4
```
出力: `Fish`

```Fish Shell
# 文字列の末尾から3文字を取り出す
echo $my_string | string sub -s -3
```
出力: `n!`

## Deep Dive (深掘り)
Fish Shellでは、`string` コマンドがバージョン 2.3.0 から利用可能です。他のシェル言語と比べ、Fishは構文が簡単で直感的。似たような処理が `cut` や `awk` コマンドで可能ですが、`string`を使う方がFishでは自然です。サブストリングの抽出は `-s`（開始位置の指定）と `-l`（長さの指定）オプションで管理します。この直感的なアプローチはプログラマーが素早く目的の文字列を操作できるように設計されています。

## See Also (関連情報)
- Fish公式ドキュメント: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- `string`コマンドについての詳細: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- シェルスクリプトチュートリアル: [https://www.learnshell.org/](https://www.learnshell.org/)
