---
title:                "デバッガーの使い方"
date:                  2024-01-26T03:50:32.396278-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？
デバッガを使用することは、基本的にあなたがコードの中で探偵になり、バグを探し出し、物事がスムーズに動かない理由を解明することです。プログラマーがこれを行う理由は、面と向かって言いましょう、バグは避けられないものであり、それらを効率的に潰すことは、コードをより速く、より確実に稼働させることを意味します。

## 方法：
Gleamは現在、ツール類に関してはErlangエコシステムに頼っているため、通常は`rebar3`、`observer`、`debugger`のようなツールを使ってデバッグします。デバッグを開始する方法は以下の通りです：

```gleam
// rebar設定内で、デバッグ情報を含めるためにこれらの行を確認する：
{erl_opts, [debug_info]}.

// アプリを読み込んだErlangシェルを実行
rebar3 shell

// シェル内で、デバッガを開始する
1> debugger:start().
```

簡単ですね？ `debugger` GUIが表示され、ブレークポイントを設定し、コードをステップ実行し、変数を監視することができます。Gleamコードを直接見ることはできませんが、それがコンパイルされたErlangコードを見ることができ、これはまだ非常に役立ちます。

## 深掘り
Gleamは若い言語なので、Erlangエコシステムの肩に乗っている間、ネイティブのGleamデバッグツールはまだスポットライトを浴びていません。つまり、われわれはErlangの信頼できる古き良きツールを使用しているわけですが、それは悪いことではありません。Erlangのデバッガは90年代から存在し、信頼性が鍵となるシステムで厄介なバグを根絶するために年月をかけて洗練されてきました。

代替手段として、トレーシングはBEAM世界（ErlangやElixirコードを実行する仮想マシン）において強力な方法です。`rebar3`を使って、関数呼び出しをトレースし、パフォーマンスの問題に深く潜るためのツールである`recon`などにアクセスできます。

Gleamの記述とErlangでのデバッグの間の切り替えは、あたかもその場で自分の考えを翻訳しているように感じるかもしれません。しかし、その見返りとして、アプリのビルディングブロックを実行形式で理解する際に、Erlangの世界を垣間見るチャンスを得られます。

## 参照
デバッグツールキットを拡張するために、次の情報をチェックしてください：

- Erlangのデバッガドキュメント：[https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- Erlang用の`recon`ライブラリ：[https://ferd.github.io/recon/](https://ferd.github.io/recon/)
