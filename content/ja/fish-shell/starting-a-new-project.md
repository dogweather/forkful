---
title:                "Fish Shell: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## なぜ？

新しいプロジェクトを始める理由はたくさんあります。新しい技術を学ぶため、興味のある分野にチャレンジするため、あるいは単に新しいアプリケーションを作りたいためなど、色々な理由があるでしょう。

## やり方

まずはFish Shellをインストールしましょう。次に、新しいプロジェクトのディレクトリを作成します。例えば、`my-project`という名前で作成します。

```
Fish Shell> mkdir my-project
```

新しいプロジェクトのディレクトリに移動し、空のファイルを作ります。ここでは、`index.js`という名前のファイルを作成します。

```
Fish Shell> cd my-project
Fish Shell> touch index.js
```

次に、作成したファイルを開き、好きなエディタでコードを書き始めます。

```
Fish Shell> nano index.js
```

例として、`Hello World`を出力するだけの簡単なプログラムを書いてみましょう。

```
console.log("Hello World");
```

上記のコードを保存してから、実行してみましょう。

```
Fish Shell> node index.js
```

ターミナルに`Hello World`というメッセージが表示されれば、プログラムが正しく実行されています。

## ディープダイブ

新しいプロジェクトを始める時、最初の一歩が重要です。プロジェクトの目的や構成をよく考えることが、成功のカギです。また、チームでプロジェクトを進める場合は、コミュニケーションを密にすることも重要です。

新しいプロジェクトを始める際、以下の点も意識しておきましょう。

- プロジェクトの目的やゴールを明確にする
- 使用する技術や言語を決める
- コーディング規約を決める
- バージョン管理システムを導入する

これらのポイントを意識することで、プロジェクトの管理や進行がスムーズになります。

## 参考リンク

- [Fish Shell公式サイト](https://fishshell.com/)
- [Fish Shellチュートリアル](https://fishshell.com/docs/current/tutorial.html)
- [Node.js公式サイト](https://nodejs.org/en/)
- [Node.jsチュートリアル](https://www.tutorialspoint.com/nodejs/index.htm)
- [GitHubの使い方](https://www.atlassian.com/git/tutorials/what-is-git)