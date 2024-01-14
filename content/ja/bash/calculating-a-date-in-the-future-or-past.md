---
title:    "Bash: 「将来または過去の日付を計算する」"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## なぜ

日常生活において、私たちは自分の生活や仕事の予定をスケジュールに基づいて計画する必要があります。しかし、時には、退屈な計算や手作業を行わなければならないこともあります。そこで、Bashプログラミングを使用して、将来や過去の日付を計算する方法を学ぶことは非常に便利です。

## 方法

まず、Bashを使用して日付を計算するには、`date`コマンドを使用します。例えば、2日後の日付を計算するには、以下のように入力します。

```
Bash ```date -d "2 days"
```

そして、出力は以下のようになります。

```
Fri Feb 5 00:00:00 JST 2021
```

さらに、一ヶ月後や前の日付を計算する方法や、特定の書式で出力する方法など、より複雑な計算をすることもできます。

## ディープダイブ

もし、さらに深く日付の計算について学びたい場合は、Bashの`date`コマンドのマニュアルを参照することをおすすめします。そこには、さまざまなオプションやフォーマット指定の方法が記載されています。また、日付と時刻のフォーマットに関するGNU dateの公式ドキュメントも参考にすることができます。

## 参考リンク

- [GNU dateの公式ドキュメント](https://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html#Date-input-formats)
- [Bashの`date`コマンドのマニュアル](https://www.gnu.org/software/coreutils/manual/html_node/Calculating-Deltas.html#Calculating-Deltas)