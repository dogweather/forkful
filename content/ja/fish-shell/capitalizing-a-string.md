---
title:    "Fish Shell: 文字列をキャピタライズする"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の大文字化に取り組む理由をご存知ですか？それでは、その理由をお伝えします。

文字列を大文字化することで、テキストのフォーマットを統一したり、データの検索や整理をしやすくしたりすることができます。また、文字列の大文字と小文字を区別する言語では、小文字を大文字に変換することによって意味が変わる場合もあります。そのため、正しい文法を維持するためにも大文字化は重要な作業です。

## 使い方

大文字化にはさまざまな方法がありますが、今回はFish Shellを使用した方法をご紹介します。Fish Shellは使いやすいコマンドラインシェルで、初心者にもおすすめのプログラミング言語です。

まず、大文字化したい文字列を選びます。次に、以下のようにコマンドを入力し、文字列を大文字化します。

```Fish Shell
set str "hello"
toupper $str
```

この例では、`hello`という文字列が`toupper`というコマンドによって大文字化され、`HELLO`という出力が得られます。

また、複数の文字列を同時に大文字化することもできます。例えば、以下のようにすることで、`hello`と`world`の両方が大文字化されます。

```Fish Shell
set strlist "hello" "world"
toupper $strlist
```

## 深堀り

Fish Shellでは、`toupper`以外にも文字列を大文字化するためのコマンドが用意されています。その中には、アクセント付き文字なども自動的に大文字化してくれる便利なものもあります。また、フォルダやファイル名などの特殊文字も適切に扱われるため、安心して大文字化を行うことができます。

さらに、Fish Shellでは`toupper`をカスタマイズすることも可能です。例えば、特定の文字を大文字化しないようにしたり、特定の場合のみ大文字化したりすることができます。

大文字化に関する詳細な情報や、他のコマンドについては、公式ドキュメントを参照するとよいでしょう。

## 参考リンク

- [Fish Shell 公式ドキュメント](https://fishshell.com/docs/current/)
- [Fish Shell インストール方法](https://fishshell.com/docs/current/#installation)
- [文字列の大文字化とは？](https://www.wikiwand.com/ja/%E5%A4%A7%E6%96%87%E5%AD%97)
- [日本語で学ぶFish Shell](https://yutora-no-fuunji.com/history)
- [Fish Shell の便利なコマンド 10 選](https://qiita.com/b4b4r07/items/09815efb9151a6b3c229)

## 関連リンク

- [ファイルの扱いに便利なFish Shellのコマンド](https://qiita.com/usagikeri/items/3be2c9c2aebe93eebc43)
- [Fish Shellを使ってみよう](https://qiita.com/po3rin/items/e00056199aa1b3795040)
- [コマンドライン入門：Fish Shellの使い方](https://qiita.com/celeron1ghz/items/d9e0ae5f23116c92bc4a)
- [Fish Shellで遊ぶ](https://qiita.com/toshiwataru/items/5948e406edf7951dacb0)
- [