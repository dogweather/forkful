---
title:    "Bash: 「未来や過去の日付の計算」"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

今日、私たちは日常生活の中でさまざまな日付を計算する必要があります。誕生日や予定されたイベントの日付を計算したり、終わったイベントの日付を振り返ったりすることがあります。Bashプログラミングで日付を計算することは、これらのタスクをより簡単にするための一つの方法です。

## 方法

Bashプログラミングで日付を計算するには、dateコマンドを使用します。例えば、2022年5月1日の1年前の日付を計算するには、以下のようにコマンドを入力します。

```Bash
date -d "1 year ago 2022-05-01"
```

そして、出力は以下のようになります。

```Bash
Sun May 2 00:00:00 JST 2021
```

このように、dateコマンドでは、指定した日付や時間から前後の日付を計算することができます。また、日付や時間のフォーマットをカスタマイズすることもできます。

## 深堀り

日付を計算する際、dateコマンドではさまざまなオプションを使用することができます。例えば、以下のオプションを使うことで、今日の日付から1週間後の日付を計算することができます。

```Bash
date -d "today + 1 week"
```

また、日付や時間のフォーマットを指定したい場合は、`-I`オプションを使用します。例えば、ISOフルフォーマットで日付を出力するには、以下のようになります。

```Bash
date -d "1 year ago 2022-05-01" -I
```

そして、出力は以下のようになります。

```Bash
2021-05-02
```

さらに、`-f`オプションを使用することで、独自の日付や時間のフォーマットを指定することもできます。詳しいオプションやコマンドの使い方については、[公式ドキュメント](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)を参考にしてください。

## 関連リンク

- [GNU Coreutils公式ドキュメント](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Bashのループ文で日付を計算する方法](https://dev.classmethod.jp/articles/bash-loop-date-calculation/)