---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
ランダム数の生成とは、偶然性に基づき不確かな数値を生み出す行為です。プログラマーがこれを行う理由は、テストデータの作成、一意のIDの生成、またはシミュレーションにおける予測不能性の導入など、多岐にわたります。

## どうやって：
以下にGleamでのランダム数の生成例を示します。

```Gleam
import gleam/otp/process.{spawn}
import gleam/atom.{Atom}
import gleam/otp/erlang

fn start() {
  let pid = spawn(fn() {
    process()
  })
}

fn process() {
  let num = erlang.random.uniform(1, 100)
  io.println(num)
}
```

このコードは、新規プロセスを生成してそのプロセス内で1から100までの間のランダムな数値をコンソールに出力します。

## ディープダイブ
ランダム数の生成は、コンピュータサイエンスの初期から存在し、その必要性は今日もなお変わりません。Gleamや他のErlang VMベースの言語では、`erlang.random.uniform`関数などのBIFs(組み込み関数)を通じて実装されます。また、`gleam/otp/erlang`ライブラリをインポートすることで利用可能となります。

代替手段としては、あらかじめ決められた長さのランダム値を生成する、より製作者が管理しやすい関数を自己定義することもできます。ただし、そのような場合でも、内部ではクリプトグラフィックに安全なRNG(ランダム数生成器)からの出力を使用するのが一般的です。

## 参考リンク
ランダム数の生成をさらに深く理解するためには以下のリンクが役立ちます。

1. Erlangのランダム数生成： https://erlang.org/doc/man/rand.html
2. Gleam言語の公式サイト： https://gleam.run/
3. Gleamのソースコード： https://github.com/gleam-lang/gleam