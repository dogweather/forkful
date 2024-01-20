---
title:                "新しいプロジェクトを始める"
html_title:           "C: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 何となぜ？

新規プロジェクトの開始とは、新しいアイディアや問題解決のためのコードを作り始めることです。プログラマーはこれを行うことで、ソフトウェアを改善し、新しい機能を開発します。

## どうやって？

はじめに、まっさらなプロジェクトを作成します。例えば、次のように`stack new myproject`を実行します。

```Haskell
$ stack new myproject
```

これで新しいHaskellプロジェクト`myproject`が作成されました。フォルダと必要なファイルが自動で生成されます。

次に、生成されたプロジェクトフォルダに移動します。

```Haskell
$ cd myproject
```

あとは`stack build`と入力し、プロジェクトをビルドします。

```Haskell
$ stack build
```

これでプロジェクトの準備が整いました。

## ディープダイブ

新規プロジェクトの開始の背後には様々な要素があります。おそらく最も重要なのは、各プロジェクトが自分自身の独立した環境を持つという概念、つまり「sandboxing」です。これは、異なるプロジェクト間でライブラリのバージョンや設定が競合しないようにするためのものです。

これに対する代替手段としては、globalにライブラリをインストールし、全てのプロジェクトで同じライブラリを共有するというやり方もありますが、sandboxingの方が衝突のリスクを遥かに低減します。

新規プロジェクトを作成するとき、`stack`はデフォルトで一連のディレクトリとファイルを生成します。もちろん、これらは後から調整やカスタマイズが可能です。

## 参考リンク

以下のリンクはさらに詳しい情報や関連リソースを提供します：

- Stack公式ドキュメンテーション: [https://docs.haskellstack.org/](https://docs.haskellstack.org/)
- Haskellプロジェクトの作成：[https://www.tutorialspoint.com/haskell/haskell_quick_guide.htm](https://www.tutorialspoint.com/haskell/haskell_quick_guide.htm)
- Haskellのビルドツールについて: [https://tech.fpcomplete.com/haskell/tutorial/stack](https://tech.fpcomplete.com/haskell/tutorial/stack)