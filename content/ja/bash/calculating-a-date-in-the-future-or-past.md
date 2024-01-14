---
title:                "Bash: 未来と過去の日付を計算する"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を未来や過去に計算する必要があるのでしょうか？例えば、誕生日やイベントの日付を計算したい場合は、今日から何日後なのかを知りたいと思うかもしれません。または、コンピューターシステムで日付を操作する必要がある場合もあります。このような場合、日付を計算することはとても役に立ちます。

## 使い方

日付を計算するためのBashプログラミングの例を紹介します。まず、未来の日付を計算する場合は、```date```コマンドを使用します。例えば、今日の日付から1ヶ月後を計算する場合は、以下のように入力します。

```Bash
date -d "+1 month"
```

出力は次のようになります。

```Bash
Tue Oct 12 00:00:00 JST 2021
```

また、過去の日付を計算する場合も同じコマンドを使用します。例えば、10日前の日付を計算する場合は、以下のように入力します。

```Bash
date -d "10 days ago"
```

出力は次のようになります。

```Bash
Mon Sep 20 00:00:00 JST 2021
```

## 詳細について

日付を計算するためには、使用できるオプションがたくさんあります。例えば、```date```コマンドには、年や月、週など、日付を計算するための異なる単位を指定するオプションがあります。また、特定の書式で日付を表示することもできます。詳細については、オンラインドキュメントを参照してください。

## 参考リンク

- [Bashの各種コマンドについてのオンラインドキュメント](https://www.gnu.org/software/bash/manual/bash.html)
- [Bashプログラミングのチュートリアル](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)
- [日付を計算するオプションの詳細](https://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html)

## その他のリンク

- [Bash公式ウェブサイト](https://www.gnu.org/software/bash/)
- [Bashコミュニティフォーラム](https://lists.gnu.org/mailman/listinfo/bug-bash)
- [Bashの最新バージョンのダウンロード](https://ftp.gnu.org/gnu/bash/)