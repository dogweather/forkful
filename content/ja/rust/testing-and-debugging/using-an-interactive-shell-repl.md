---
date: 2024-01-26 04:18:24.997511-07:00
description: "Rust \u306E\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\
  \u30EB\u3001\u307E\u305F\u306FREPL\uFF08Read-Eval-Print\u30EB\u30FC\u30D7\uFF09\u306F\
  \u3001Rust\u2026"
lastmod: '2024-03-13T22:44:41.824594-06:00'
model: gpt-4-0125-preview
summary: "Rust \u306E\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\
  \u3001\u307E\u305F\u306FREPL\uFF08Read-Eval-Print\u30EB\u30FC\u30D7\uFF09\u306F\u3001\
  Rust\u2026"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？
Rust のインタラクティブシェル、またはREPL（Read-Eval-Printループ）は、Rust コードをその場で実行し、即座に結果を見ることができます。これは、実験や学習に最適です。プログラマーは、プロジェクト全体をコンパイルする手間なしに、コードスニペットのテスト、デバッグ、あるいは言語機能を試すために使用します。

## どうやって：
現在のところ、Rustには公式のREPLが標準装備されていません。`evcxr_repl`のようなサードパーティのツールを使用できます。Cargoでインストールします：

```sh
cargo install evcxr_repl
```

その後、REPLを実行します：

```sh
evcxr
```

内部で、いくつかのRustコードをテストします：

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

出力は次のようになります：

```
5 + 3 = 8
```

## より深く
Rustの倫理思考は、安全性とパフォーマンスを中心に据えており、通常、コンパイル前言語に関連付けられていますが、解釈型でREPLに優しい言語とはあまり関係がありません。歴史的には、PythonやRubyのような言語は即時フィードバックのためにREPLを優先しましたが、システムレベルのタスクを念頭に置いて設計されたわけではありません。

Rustに公式のREPLがないにもかかわらず、`evcxr_repl`のようないくつかの代替品が登場しました。これらのプロジェクトは、RustをREPLに単に組み込んでいるだけでなく、言語のコンパイル・アンド・ランサイクルをインタラクティブなセッションに巧みに織り交ぜています。REPLは、コードを舞台裏でコンパイルし、バイナリを実行し、出力をキャプチャします。この方法により、インタラクティブな体験を提供しながらも、Rustのパフォーマンスの利点を保持しています。

Rustコミュニティ内では公式のREPLサポートについての議論が進行中であり、言語の各イテレーションで、最終的にネイティブソリューションにつながるかもしれないより洗練されたツールが登場しています。

## 参照
詳細および他のツールについては：
- Evcxr REPL GitHubリポジトリ：[https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground、Rustコードをテストするオンライン方法：[https://play.rust-lang.org/](https://play.rust-lang.org/)
- REPL機能に関するRust言語議論：[https://internals.rust-lang.org/](https://internals.rust-lang.org/)
