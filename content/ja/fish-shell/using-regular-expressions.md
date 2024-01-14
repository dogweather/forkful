---
title:    "Fish Shell: 正規表現の使い方"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ正規表現を使うのか？

正規表現は特定のパターンに一致する文字列を検索・置換するための強力なツールです。テキスト処理やデータ検索など、様々な場面で使われています。

## 正規表現を使う方法

正規表現を使うには、まずFish Shellで`grep`コマンドを使います。例えば、あるファイル内のメールアドレスを検索したい場合は、次のように入力します。

```
Fish Shell:
grep --color -E '[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}' test.txt
```

上記の例では、`grep`コマンドの`-E`オプションを使って正規表現モードを有効にしています。`[a-z0-9._%+-]+`はメールアドレスのローカル部分を、`[a-z0-9.-]+\.[a-z]{2,}`はドメイン部分を表しています。

実行すると、`test.txt`ファイル内からメールアドレスのパターンに一致する行がカラー表示で出力されます。

## 正規表現の深層へ

正規表現を使う上で重要なポイントは、パターンの記述です。例えば、`[0-9]`と書くと、1文字だけ数字にマッチしますが、`[0-9]+`と書くと連続する数字にマッチします。また、`*`や`?`などのメタ文字を使うことでパターンの組み合わせをさらに自由にカスタマイズすることができます。

さらに、`|`パイプを使うことで複数のパターンのいずれかにマッチさせることもできます。正規表現を使いこなすためには、パターンとメタ文字の使い方を熟知することが重要です。

## 別の参考文献

- [Fish Shellの公式ドキュメント](https://www.fishshell.com/docs/current/scripting.html#regular-expressions)
- [正規表現チートシート](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [正規表現ビルダー](https://regexr.com/)

## リンク

- [grepコマンドのドキュメント](https://www.gnu.org/software/grep/manual/grep.html)
- [正規表現の基礎](https://www.w3schools.com/python/python_regex.asp)
- [よく使われる正規表現パターンの例](https://www.geeksforgeeks.org/important-regular-expressions/)