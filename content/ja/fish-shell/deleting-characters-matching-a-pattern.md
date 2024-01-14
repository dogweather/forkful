---
title:    "Fish Shell: パターンにマッチする文字を削除する"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

コンピューターで作業している際に、必要のない文字や情報が表示されることはよくあります。その際、特定のパターンにマッチする文字を削除することで、スクリーン上のいらない情報を取り除くことができます。この記事では、Fish Shellを使って特定のパターンにマッチする文字を削除する方法を紹介します。

## 方法

まずは、Fish Shellをインストールします。次に、以下の例を参考にして、削除したいパターンにマッチする文字を指定します。

```Fish Shell
grep -lv '[pattern]' *
```
上記の例では、`-l`オプションを使うことで、パターンにマッチしない行を表示します。`-v`オプションを使うことで、指定したパターンにマッチする行を表示せず、その他の行だけを表示します。`*`はファイル名を表し、カレントディレクトリにあるファイルを対象にします。

例えば、特定のパターンにマッチする行を持つファイルを削除したい場合は、次のようにします。

```Fish Shell
rm $(grep -l '[pattern]' *)
```

上記の例では、`-l`オプションを使い、パターンにマッチする行を持つファイルのリストを表示します。それを`rm`コマンドで削除することができます。

## ディープダイブ

特定のパターンにマッチする文字を削除する方法はさまざまありますが、核心は`grep`コマンドを使って特定のパターンにマッチする行を表示し、それを基に操作を行うことです。`grep`コマンドにはさまざまなオプションがありますので、より詳細な処理を行いたい場合は、マニュアルを参照してください。

## 今後の参考になる記事

- [Fish Shell公式サイト](https://fishshell.com/)
- [Fish Shellのマニュアル](https://fishshell.com/docs/current/)
- [正規表現の一覧](https://docs.oracle.com/javase/tutorial/essential/regex/index.html)

お疲れ様でした！この記事を参考にして、あなたの作業をより効率的に行いましょう。