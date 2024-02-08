---
title:                "パターンに一致する文字を削除する"
aliases:
- ja/fish-shell/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:13.581501-07:00
model:                 gpt-4-1106-preview
simple_title:         "パターンに一致する文字を削除する"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
パターンにマッチする文字の削除は、特定の条件に合う文字列を取り除く処理です。プログラマは、不要なデータをクリアしたり、フォーマットを整えたりするためにこの操作を行います。

## How to (やり方)
```Fish Shell
# 文字列からパターンに一致する部分を削除する
echo "fish_shell_rocks" | string replace -r '_.*' ''
# 出力: fish

# 複数のパターンにマッチする文字を削除
echo "remove vowels from this sentence." | string replace -a 'a' '' | string replace -a 'e' '' | string replace -a 'i' '' | string replace -a 'o' '' | string replace -a 'u' ''
# 出力: rmv vwls frm ths sntnc.

# 特定のファイルからパターンにマッチする行を削除
string match -v "pattern" < file.txt > new_file.txt
# 'file.txt'から'pattern'に一致する行を削除して、'new_file.txt'に保存します。
```

## Deep Dive (深掘り)
Fish Shellにおけるパターンマッチは、正規表現を用いて柔軟な文字列操作を可能にします。伝統的なUnix Shellと比較して、シンタックスが直感的で、独自のコマンドを使うことで複雑なパターンも容易に扱えます。他のシェルスクリプティング言語と比較して、Fishはユーザーフレンドリーでスクリプトが読みやすくなるよう設計されていますが、一部POSIX互換でないことから他のシェルとの移行には注意が必要です。`string`コマンドはFish Shellで追加された機能で、sedやawkといった従来のUnixコマンドに代わる選択肢を提供しています。これにより、パイプラインを通して効率的に文字列処理を行えるようになります。

## See Also (関連情報)
- [Fish Shell Documentation - String](https://fishshell.com/docs/current/commands.html#string)
- [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [Regular Expressions in Fish](https://fishshell.com/docs/current/index.html#regexp)
