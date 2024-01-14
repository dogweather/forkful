---
title:    "Fish Shell: textを検索して置換する"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ
テキストの検索と置換を行う理由は、大量のテキストを処理するときにとても便利です。これは特にプログラマーやデータ分析者にとって有用です。たとえば、特定のキーワードを持つファイルや行だけを抽出するために使用できます。

## ハウツー
Fishシェルでテキストを検索して置換する方法を示します。まず、検索するキーワードと置換するテキストを指定します。次に、`sed`コマンドを使用して置換を実行します。

```Fish Shell
# 検索するキーワードと置換するテキストを指定する
set keyword "こんにちは"
set replacement "Hello"
# 置換を実行する
cat example.txt | sed "s/$keyword/$replacement/g"
```

上記の例では、`example.txt`というファイルからキーワードが「こんにちは」の行を「Hello」に置換しています。`sed`コマンドの`g`オプションを使用することで、すべてのキーワードを同時に置換することができます。

## ディープダイブ
`sed`コマンドの機能をより深く掘り下げてみましょう。`s`コマンドを使用することで、キーワードを特定のパターンにマッチさせることができます。`&`という特殊なパラメータを使用することで、マッチしたテキスト全体を置換することができます。また、正規表現を使用したり、置換をファイルに保存することもできます。

## 関連リンクを参照
- [Fish Shell公式ドキュメント](https://fishshell.com/docs/current/cmds/sed.html)
- [Sedとは？基本的な使い方を徹底解説！](https://it-bank.co.jp/adult/qa/systemadmin/sed-2481/)
- [Fish Shellのチートシート](https://devhints.io/fish-shell)