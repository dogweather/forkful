---
title:                "エラー処理"
date:                  2024-01-26T00:53:20.049321-07:00
model:                 gpt-4-1106-preview
simple_title:         "エラー処理"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/handling-errors.md"
---

{{< edit_this_page >}}

## 何となぜか？
エラーを扱うということは、コード内で物事がうまくいかないことを予測し、それらの状況を上手く管理することを意味します。プログラマーがこれを行うのは、予期せぬことに直面したときでも、アプリケーションが堅牢でユーザーフレンドリーであるようにするためです。

## どうやって：
Gleamではエラー処理に`Result`型をよく使用します。これは`Ok`（成功の場合）と`Error`（失敗の場合）の2つのバリアントを持つenumです。こちらが簡単な例です：

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("オップス！壊れました。".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(value) => value
    Error(message) => {
      io.println(message)
      0
    } 
  }
}
```

`main`を`might_fail(False)`で実行すると`42`を返します。`True`を渡すと"オップス！壊れました。"と表示され、`0`が返されます。

## 詳細解説
Gleamのエラー処理アプローチは、そのErlangのルーツに影響を受けています。歴史的にErlangは「クラッシュさせる」という哲学を使用しており、プロセスの失敗を監視ツリーで管理することに頼っています。しかし、監視されるべきプロセス以外でGleamコードを書く時、例えばライブラリー関数内で、エラーを明示的に処理したいでしょう。

`Result`を使用する代わりの方法としては、何もない場合は`None`、何かがある場合は`Some`となる`Option`型を使用する場合がありますが、これらはエラー情報を伝えません。プロセスの境界を越えてエラーを通知するためには、Erlangのメッセージパッシング機構を使用することもあります。

Gleamのエラー処理は、副作用（例えばエラー）を型やパターンマッチングで管理する関数型プログラミングスタイルを反映しており、エラー管理において明確さと予測可能性を提供します。

## 関連項目
- [Erlangのエラー処理](http://erlang.org/doc/reference_manual/errors.html)
